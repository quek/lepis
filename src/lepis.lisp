(in-package :lepis)

(defvar *db* nil)

(sb-ext:defglobal *db-table* (make-hash-table :synchronized t :weakness :value :test 'equal))

(defstruct (db (:constructor %open-db))
  (hash (make-hash-table :synchronized t :test 'equal))
  (update-count 0 :type fixnum)
  (dump-threshold-second 60 :type fixnum)
  (last-dumped-time (get-universal-time))
  data-dir
  dump-thread
  (open-count 1 :type fixnum)
  lock-fd)

(defun db-dump-file (db)
  (merge-pathnames "dump.lisp" (db-data-dir db)))

(defun open-db (data-dir &key (dump-threshold-second 60))
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
                                 :lock-fd fd))
              (setf (gethash data-dir *db-table*) db)
              (when (probe-file (db-dump-file db))
                (load-db db))
              (let ((thread (sb-thread:make-thread
                             #'dump-thread
                             :arguments (list db)
                             :name (format nil "dump-thread ~a" (db-dump-file db)))))
                (setf (db-dump-thread db) thread)
                (sb-ext:finalize db (lambda ()
                                      (sb-thread:terminate-thread thread))))))))))

(defun dump-thread (db)
  (loop (sleep (db-dump-threshold-second db))
        (when (plusp (db-update-count db))
          (setf (db-update-count db) 0)
          (ignore-errors (fork-and-dump db)))))

(defun close-db (&optional (db *db*))
  (sb-ext:with-locked-hash-table (*db-table*)
    (if (zerop (decf (db-open-count db)))
        (progn
          (remhash (db-data-dir db) *db-table*)
          (sb-thread:terminate-thread (db-dump-thread db))
          (dump-db db)
          (unlock-file (db-lock-fd db))
          t)
        nil)))

(defmacro with-db ((data-dir) &body body)
  `(let ((*db* (open-db ,data-dir)))
     (unwind-protect (progn ,@body)
       (close-db))))

(defun clear-db (&optional (db *db*))
  (clrhash (db-hash db)))

(defmacro def-read-op (op (hash &rest args) &body body)
  `(defun ,op ,args
     (let ((,hash (db-hash *db*)))
       ,@body)))

(defmacro def-write-op (op (hash &rest args) &body body)
  `(defun ,op ,args
     (let ((,hash (db-hash *db*)))
       (sb-ext:with-locked-hash-table (,hash)
         (incf (db-update-count *db*))
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; object

(def-read-op @ (hash key &optional default)
  (gethash key hash default))

(def-write-op ! (hash key value)
  (setf (gethash key hash) value))

(def-write-op inc (hash key &optional (delta 1))
  (incf (gethash key hash 0) delta))


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

(def-read-op zrang (hash key start stop &key with-scores)
  (let ((zset (gethash key hash)))
    (when zset
      (zset-range zset start stop with-scores))))

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
  (let ((file (multiple-value-bind (_ file)
                  (sb-posix:mkstemp
                   (namestring
                    (merge-pathnames "dump-temp-XXXXXX" (db-data-dir db))))
                (declare (ignore _))
                file)))
    (with-open-file (out file :direction :output :if-exists :overwrite)
      (with-standard-io-syntax
        (sb-ext:with-locked-hash-table ((db-hash db))
          (setf (db-update-count db) 0)
          (dump-db-hash (db-hash db) out))
        (multiple-value-bind (s mi h d m y) (decode-universal-time (get-universal-time))
          (format out "~%;; ~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" y m d h mi s)))
      (rename-file file (db-dump-file db)))))

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
      (let ((hash (db-hash db)))
        (clrhash hash)
        (load-db-hash hash in)))))

