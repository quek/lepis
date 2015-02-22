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

(defun open-db (data-dir)
  (ensure-directories-exist data-dir)
  (aprog1 (%open-db :data-dir data-dir)
    (when (probe-file (db-dump-file it))
      (load-db it))
    (let ((thread (sb-thread:make-thread
                   (lambda (db)
                     (loop (sleep (db-dump-threshold-second db))
                           (dump-db db)))
                   :arguments (list it)
                   :name (format nil "dump-thread ~a" (db-dump-file it)))))
      (setf (db-dump-thread it) thread)
      (sb-ext:finalize it (lambda ()
                            (sb-thread:terminate-thread thread))))))

(defun close-db (db)
  (sb-thread:terminate-thread (db-dump-thread db))
  (dump-db db))

(defmacro with-db ((var data-dir) &body body)
  `(let ((,var (open-db ,data-dir)))
     (unwind-protect (progn ,@body)
       (close-db ,var))))

(defun clear-db (db)
  (clrhash (db-hash db)))

(defun @ (db key &optional default)
    (gethash key (db-hash db) default))

(defmacro def-update-op (op (db hash &rest args) &body body)
  `(defun ,op (,db ,@args)
     (let ((,hash (db-hash ,db)))
       (sb-ext:with-locked-hash-table (,hash)
         ,@body))))

(def-update-op ! (db hash key value)
  (setf (gethash key hash) value))

(def-update-op inc (db hash key &optional (delta 1))
  (incf (gethash key hash 0) delta))


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
          (maphash (lambda (key value)
                     (print key out)
                     (print value out))
                   (db-hash db))))
      (rename-file file (db-dump-file db)))))

(defun load-db (db)
  (with-open-file (in (db-dump-file db))
    (with-standard-io-syntax
      (let ((hash (db-hash db))
            (*read-eval* nil))
        (clrhash hash)
        (loop for key = (read in nil in)
              for value = (read in nil in)
              while (not (eq in key))
              do (setf (gethash key hash) value))))))

