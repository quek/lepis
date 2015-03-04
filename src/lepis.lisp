(in-package :lepis)

(defstruct (db (:constructor %open-db))
  (hash (make-hash-table :synchronized t :test 'equal))
  (update-count 0)
  (dump-threshold-second 60)
  (last-dumped-time (get-universal-time))
  data-dir
  dump-thread)

(defun db-dump-file (db)
  (merge-pathnames "dump.lisp" (db-data-dir db)))

(defun open-db (data-dir &key (dump-threshold-second 60))
  (ensure-directories-exist data-dir)
  (aprog1 (%open-db :data-dir data-dir :dump-threshold-second dump-threshold-second)
    (when (probe-file (db-dump-file it))
      (load-db it))
    (let ((thread (sb-thread:make-thread
                   #'dump-thread
                   :arguments (list it)
                   :name (format nil "dump-thread ~a" (db-dump-file it)))))
      (setf (db-dump-thread it) thread)
      (sb-ext:finalize it (lambda ()
                            (sb-thread:terminate-thread thread))))))

(defun dump-thread (db)
  (loop (sleep (db-dump-threshold-second db))
        (when (plusp (db-update-count db))
          (setf (db-update-count db) 0)
          (ignore-errors (fork-and-dump db)))))

(defun close-db (db)
  (sb-thread:terminate-thread (db-dump-thread db))
  (dump-db db))

(defmacro with-db ((var data-dir) &body body)
  `(let ((,var (open-db ,data-dir)))
     (unwind-protect (progn ,@body)
       (close-db ,var))))

(defun clear-db (db)
  (clrhash (db-hash db)))

(defmacro def-read-op (op (db hash &rest args) &body body)
  `(defun ,op (,db ,@args)
     (let ((,hash (db-hash ,db)))
       ,@body)))

(defmacro def-write-op (op (db hash &rest args) &body body)
  `(defun ,op (,db ,@args)
     (let ((,hash (db-hash ,db)))
       (sb-ext:with-locked-hash-table (,hash)
         (incf (db-update-count ,db))
         ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; object

(def-read-op @ (db hash key &optional default)
  (gethash key hash default))

(def-write-op ! (db hash key value)
  (setf (gethash key hash) value))

(def-write-op inc (db hash key &optional (delta 1))
  (incf (gethash key hash 0) delta))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hash

(def-read-op hget (db hash key &rest fields)
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

(def-write-op hdel (db hash key field &rest more-fields)
  (let ((h (gethash key hash)))
    (if h
        (loop for f in (cons field more-fields)
              count (remhash f h))
        0)))

(def-write-op hset (db hash key field value &rest more-field-values)
  (let ((h (gethash key hash)))
    (unless h
      (setf h (setf (gethash key hash) (make-hash-table :test 'equalp))))
    (loop for (f v) on (cons field (cons value more-field-values)) by #'cddr
          do (setf (gethash f h) v))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zset

(def-write-op zadd (db hash key score member &rest more-score-members)
  (let ((zset (gethash key hash)))
    (unless zset
      (setf zset (setf (gethash key hash) (make-zset))))
    (apply #'zset-add zset score member more-score-members)))

(def-read-op zrang (db hash key start stop &key with-scores)
  (let ((zset (gethash key hash)))
    (zset-range zset start stop with-scores)))

(def-read-op zrang-by-score (db hash key min max &key with-scores (offset 0) limit)
  (let ((zset (gethash key hash)))
    (zset-range-by-score zset min max with-scores offset limit)))

(def-read-op zcard (db hash key)
  (aif (gethash key hash)
       (zset-card it)
       0))

(def-read-op zrank (db hash key member)
  (zset-rank (gethash key hash) member))

(def-write-op zrem (db hash key member &rest more-members)
  (let ((zset (gethash key hash)))
    (if zset
        (apply #'zset-delete zset member more-members)
        0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set

(def-write-op sadd (db hash key value &rest more-values)
  (let ((set (gethash key hash)))
    (unless set
      (setf set (setf (gethash key hash) (make-set))))
    (loop for x in (cons value more-values)
          count (set-add set x))))

(def-write-op srem (db hash key value &rest more-values)
  (let ((set (gethash key hash)))
    (if set
        (loop for x in (cons value more-values)
              count (set-remove set x))
        0)))

(def-read-op scard (db hash key)
  (let ((set (gethash key hash)))
    (if set
        (let ((count 0))
          (map-set (lambda (value)
                     (declare (ignore value))
                     (incf count))
                   set)
          count)
        0)))

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
          (maphash (lambda (key value)
                     (print key out)
                     (print value out))
                   (db-hash db)))
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
        (loop for key = (read in nil in)
              for value = (read in nil in)
              while (not (eq in key))
              do (setf (gethash key hash) value))))))

