(in-package :lepis)

(defvar *db* nil)

(sb-ext:defglobal *db-table* (make-hash-table :synchronized t :weakness :value :test 'equal))

(defstruct (db (:constructor %open-db))
  (hash (make-hash-table :synchronized t :test 'equal))
  (expire-hash (make-hash-table :synchronized t :test 'equal))
  (update-count 0 :type fixnum)
  (dump-threshold-second 60 :type fixnum)
  (last-dumped-time (get-universal-time))
  data-dir
  dump-thread
  expire-thread
  (expire-period-second 60 :type fixnum)
  (open-count 1 :type fixnum)
  lock-fd)

(defun db-dump-file (db)
  (merge-pathnames "dump.lisp" (db-data-dir db)))

(defun open-db (data-dir &key (dump-threshold-second 60) (expire-period-second 60))
  (sb-ext:with-locked-hash-table (*db-table*)
    (let ((db (gethash data-dir *db-table*)))
      (if db
          (progn
            (incf (db-open-count db))
            db)
          (progn
            (ensure-directories-exist data-dir)
            (let ((fd (lock-file (merge-pathnames "lock" data-dir))))
              (unless fd
                (error "Db file is opened by other process."))
              (setf db (%open-db :data-dir data-dir
                                 :dump-threshold-second dump-threshold-second
                                 :expire-period-second expire-period-second
                                 :lock-fd fd))
              (setf (gethash data-dir *db-table*) db)
              (when (probe-file (db-dump-file db))
                (load-db db))
              (let ((dump-thread (sb-thread:make-thread
                                  #'dump-thread
                                  :arguments (list db)
                                  :name (format nil "dump-thread ~a" (db-dump-file db))))
                    (expire-thread (sb-thread:make-thread
                                    #'expire-thread
                                    :arguments (list db)
                                    :name "expire-thread")))
                (setf (db-dump-thread db) dump-thread)
                (setf (db-expire-thread db) expire-thread)
                (sb-ext:finalize db (lambda ()
                                      (sb-thread:terminate-thread expire-thread)
                                      (sb-thread:terminate-thread dump-thread))))))))))

(defun dump-thread (db)
  (loop (unwind-protect
             (sleep (db-dump-threshold-second db))
          (sb-ext:with-locked-hash-table (*db-table*)
            (when (plusp (db-update-count db))
              (setf (db-update-count db) 0)
              (ignore-errors (fork-and-dump db)))))))

(defun expire-thread (db)
  (loop
    (sleep (db-expire-period-second db))
    (let ((now (get-universal-time))
          (expire-hash (db-expire-hash db))
          (hash (db-hash db)))
      (maphash (lambda (key time)
                 (declare (ignore time))
                 (when (expiredp expire-hash key now)
                   (sb-ext:with-locked-hash-table (hash)
                     (when (expiredp expire-hash key now)
                       (remhash key hash)
                       (remhash key expire-hash)))))
               expire-hash))))

(defun close-db (&optional (db *db*))
  (sb-ext:with-locked-hash-table (*db-table*)
    (if (zerop (decf (db-open-count db)))
        (progn
          (remhash (db-data-dir db) *db-table*)
          (sb-thread:terminate-thread (db-expire-thread db))
          (sb-thread:terminate-thread (db-dump-thread db))
          (dump-db db)
          (unlock-file (db-lock-fd db))
          t)
        nil)))

(defmacro with-db ((data-dir &rest args) &body body)
  `(let ((*db* (open-db ,data-dir ,@args)))
     (unwind-protect (progn ,@body)
       (close-db))))

(defun clear-db (&optional (db *db*))
  (clrhash (db-hash db)))

(declaim (inline expiredp))
(defun expiredp (expire-hash key now)
  (<= (gethash key expire-hash (1+ now)) now))

(defmacro def-read-op (op (hash key &rest args) &body body)
  (let ((expire-hash (gensym "EXPIRE-HASH"))
        (now (gensym "NOW")))
    `(defun ,op (,key ,@args)
       (let ((,hash (db-hash *db*))
             (,expire-hash (db-expire-hash *db*))
             (,now (get-universal-time)))
         (when (expiredp ,expire-hash ,key ,now)
           (sb-ext:with-locked-hash-table (,hash)
             (when (expiredp ,expire-hash ,key ,now)
               (remhash ,key ,hash)
               (remhash ,key ,expire-hash))))
         ,@body))))

(defmacro def-write-op (op (hash key &rest args) &body body)
  (let ((expire-hash (gensym "EXPIRE-HASH"))
        (now (gensym "NOW")))
    `(defun ,op (,key ,@args)
       (let ((,hash (db-hash *db*))
             (,expire-hash (db-expire-hash *db*))
             (,now (get-universal-time)))
         (sb-ext:with-locked-hash-table (,hash)
           (when (expiredp ,expire-hash ,key ,now)
             (remhash ,key ,hash)
             (remhash ,key ,expire-hash))
           (incf (db-update-count *db*))
           ,@body)))))

(defmacro def-write-op-reset-expire (op (hash key &rest args) &body body)
  `(defun ,op (,key ,@args)
     (let ((,hash (db-hash *db*)))
       (sb-ext:with-locked-hash-table (,hash)
         (remhash key (db-expire-hash *db*))
         (incf (db-update-count *db*))
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; object

(def-read-op @ (hash key &optional default)
  (gethash key hash default))

(def-write-op-reset-expire ! (hash key value)
  (setf (gethash key hash) value))


(def-write-op inc (hash key &optional (delta 1))
  (incf (gethash key hash 0) delta))

(def-write-op-reset-expire del (hash key)
  (remhash key hash))

(defun expire (key time)
  (let ((expire-hash (db-expire-hash *db*)))
    (sb-ext:with-locked-hash-table (expire-hash)
      (setf (gethash key expire-hash)
            (+ (get-universal-time) time)))))

(defun keys (&optional pred)
  (let ((pred (etypecase pred
                (null
                 (constantly t))
                (string
                 (let ((reg (ppcre:create-scanner pred)))
                   (lambda (key)
                     (ppcre:scan reg (princ-to-string key)))))
                (function
                 pred))))
    (loop for key being the hash-keys of (db-hash *db*)
          if (funcall pred key)
            collect key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hash

(def-read-op hget (hash key &rest fields)
  (let ((h (gethash key hash)))
    (when h
      (cond ((null fields)
             h)
            ((null (cdr fields))
             (gethash (car fields) h))
            (t
             (loop with r = (make-hash-table :test 'equalp)
                   for f in fields
                   do (setf (gethash f r) (gethash f h))
                   finally (return r)))))))

(def-write-op hdel (hash key field &rest more-fields)
  (let ((h (gethash key hash)))
    (if h
        (loop for f in (cons field more-fields)
              count (remhash f h))
        0)))

(def-write-op hset (hash key field value &rest more-field-values)
  (let ((h (gethash key hash)))
    (unless h
      (setf h (setf (gethash key hash) (make-hash-table :test 'equalp))))
    (loop for (f v) on (cons field (cons value more-field-values)) by #'cddr
          do (setf (gethash f h) v))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zset

(def-write-op zadd (hash key score member &rest more-score-members)
  (let ((zset (gethash key hash)))
    (unless zset
      (setf zset (setf (gethash key hash) (make-zset))))
    (apply #'zset-add zset score member more-score-members)))

(def-read-op zrang (hash key start stop &key with-scores from-end)
  (let ((zset (gethash key hash)))
    (when zset
      (zset-range zset start stop with-scores from-end))))

(def-read-op zrang-by-score (hash key min max &key with-scores (offset 0) limit)
  (let ((zset (gethash key hash)))
    (when zset
      (zset-range-by-score zset min max with-scores offset limit))))

(def-read-op zcard (hash key)
  (aif (gethash key hash)
       (zset-card it)
       0))

(def-read-op zrank (hash key member)
  (zset-rank (gethash key hash) member))

(def-write-op zrem (hash key member &rest more-members)
  (let ((zset (gethash key hash)))
    (if zset
        (apply #'zset-delete zset member more-members)
        0)))

(def-write-op zinc (hash key member &optional (delta 1))
  (let ((zset (gethash key hash)))
    (unless zset
      (setf zset (setf (gethash key hash) (make-zset))))
    (zset-inc zset member delta)))

(def-read-op zscore (hash key member)
  (aif (gethash key hash)
       (zset-score it member)))

(defun %zinter (dest-zset key.weight-list aggregate filter)
  (let ((k.w (loop for i in key.weight-list
                   for zset = (if (consp i)
                                  (@ (car i))
                                  (@ i))
                   unless zset
                     do (return-from %zinter nil)
                   collect (if (consp i)
                               (cons zset (cadr i))
                               (cons zset 1)))))
    (zset-interstore dest-zset k.w aggregate filter)
    (zset-card dest-zset)))

(def-write-op zinterstore (hash dest key.weight-list &key (aggregate #'+) filter)
  (let ((dest-zset (setf (gethash dest hash) (make-zset))))
    (%zinter dest-zset key.weight-list aggregate filter)))

(def-write-op zinter (hash key.weight-list &key (aggregate #'+) filter
                           (start 0) stop with-scores from-end)
  (let ((dest-zset (make-zset)))
    (%zinter dest-zset key.weight-list aggregate filter)
    (zset-range dest-zset start stop with-scores from-end)))

(defun %zunion (dest-zset key.weight-list aggregate filter)
  (let ((k.w (loop for i in key.weight-list
                   for zset = (if (consp i)
                                  (@ (car i))
                                  (@ i))
                   if zset
                     collect (if (consp i)
                                 (cons zset (cadr i))
                                 (cons zset 1)))))
    (zset-unionstore dest-zset k.w aggregate filter)
    (zset-card dest-zset)))

(def-write-op zunionstore (hash dest key.weight-list &key (aggregate #'+) filter)
  (let ((dest-zset (setf (gethash dest hash) (make-zset))))
    (%zunion dest-zset key.weight-list aggregate filter)))

(def-write-op zunion (hash key.weight-list &key (aggregate #'+) filter
                           (start 0) stop with-scores from-end)
  (let ((dest-zset (make-zset)))
    (%zunion dest-zset key.weight-list aggregate filter)
    (zset-range dest-zset start stop with-scores from-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set

(def-write-op sadd (hash key value &rest more-values)
  (let ((set (gethash key hash)))
    (unless set
      (setf set (setf (gethash key hash) (make-set))))
    (loop for x in (cons value more-values)
          count (set-add set x))))

(def-write-op srem (hash key value &rest more-values)
  (let ((set (gethash key hash)))
    (if set
        (loop for x in (cons value more-values)
              count (set-remove set x))
        0)))

(def-read-op scard (hash key)
  (let ((set (gethash key hash)))
    (if set
        (let ((count 0))
          (map-set (lambda (value)
                     (declare (ignore value))
                     (incf count))
                   set)
          count)
        0)))

(macrolet ((def-s-op (name op)
             `(def-read-op ,name (hash key &rest keys)
                (let ((set (gethash key hash))
                      (sets (loop for key in keys
                                  for set = (gethash key hash)
                                  if set collect set)))
                  (apply (function ,op) set sets)))))
  (def-s-op sdiff set-diff)
  (def-s-op sinter set-inter)
  (def-s-op sunion set-union))

(def-read-op smembers (hash key)
  (let ((set (gethash key hash)))
    (when set
      (lepis.set::as-list set))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dump & load
(defun dump-db (db)
  (let ((file (tempfile (db-data-dir db)))
        (hash (db-hash db)))
    (with-open-file (out file :direction :output :if-exists :overwrite)
      (with-standard-io-syntax
        (sb-ext:with-locked-hash-table (hash)
          (setf (db-update-count db) 0)
          (dump-db-hash hash (db-expire-hash db) out))
        (multiple-value-bind (s mi h d m y) (decode-universal-time (get-universal-time))
          (format out "~%;; ~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" y m d h mi s))))
    (rename-file file (db-dump-file db))))

(defun fork-and-dump (db)
  (let ((pid (sb-posix:posix-fork)))
    (if (zerop pid)
        (progn
          (dump-db db)
          (sb-posix:_exit 0))
        (sb-posix:waitpid pid 0))))

(defun load-db (db)
  (with-open-file (in (db-dump-file db))
    (with-standard-io-syntax
      (let ((hash (db-hash db))
            (expire-hash (db-expire-hash db)))
        (sb-ext:with-locked-hash-table (hash)
          (setf (db-update-count db) 0)
          (clrhash hash)
          (clrhash expire-hash)
          (load-db-hash hash expire-hash in))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-temp-keys ((&rest keys) &body body)
  `(alexandria:with-unique-names ,keys
     (unwind-protect
          (progn ,@body)
       (progn
         ,@(mapcar (lambda (key)
                     `(del ,key))
                   keys)))))
