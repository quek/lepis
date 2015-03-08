(in-package :lepis)

(defconstant +dump-end-of-object-mark+ '+dump-end-of-object-mark+)
(defconstant +dump-zset-mark+ '+dump-zset-mark+)
(defconstant +dump-set-mark+ '+dump-set-mark+)
(defconstant +dump-hash-mark+ '+dump-hash-mark+)

(defvar *dump-objects* nil)

(defun make-object-table (hash)
  (let ((table (make-hash-table :test 'eql)))
    (maphash (lambda (key value)
               (store-object key table)
               (store-value-object value table))
             hash)
    table))

(defmethod store-object (object hash)
  (typecase object
    ((or symbol number string)
     nil)
    (t
     (sunless (gethash object hash)
       (setf it (hash-table-count hash))))))

(defmacro with-value-type-case (var &key zset set hash else)
  `(cond ((lepis.zset:zset-p ,var)
          ,zset)
         ((lepis.set:set-p ,var)
          ,set)
         ((hash-table-p ,var)
          ,hash)
         (t
          ,else)))

(defun store-value-object (object hash)
  (with-value-type-case object
    :zset (maphash (lambda (key value)
                     (declare (ignore value))
                     (store-object key hash))
                   (lepis.zset::zset-hash object))
    :set (lepis.set:map-set (lambda (value)
                              (store-object value hash))
                object)
    :hash (maphash (lambda (key value)
                     (store-object key hash)
                     (store-object value hash))
                   object)
    :else (store-object object hash)))


(defun emit-object (object table stream)
  (aif (gethash object table)
       (progn
         (write-string "#." stream)
         (print `(l ,it) stream))
       (print object stream)))

(defun emit-value-object (object table stream)
  (with-value-type-case object
    :zset (progn
            (print +dump-zset-mark+ stream)
            (maphash (lambda (key score)
                       (emit-object key table stream)
                       (emit-object score table stream))
                     (lepis.zset::zset-hash object))
            (print +dump-end-of-object-mark+ stream)
            (print +dump-end-of-object-mark+ stream))
    :set (progn
           (print +dump-set-mark+ stream)
           (lepis.set:map-set (lambda (value)
                                (emit-object value table stream))
                  object)
           (print +dump-end-of-object-mark+ stream))
    :hash (progn
            (print +dump-hash-mark+ stream)
            (maphash (lambda (key value)
                       (emit-object key table stream)
                       (emit-object value table stream))
                     object)
            (print +dump-end-of-object-mark+ stream)
            (print +dump-end-of-object-mark+ stream))
    :else (emit-object object table stream)))

(defun dump-db-hash (hash stream)
  (let ((table (make-object-table hash)))
    (maphash (lambda (key value)
               (print value stream)
               (print key stream))
             table)
    (print +dump-end-of-object-mark+ stream) ;for key
    (print +dump-end-of-object-mark+ stream) ;for value
    (maphash (lambda (key value)
               (emit-object key table stream)
               (emit-value-object value table stream))
             hash)))

(defun l (id)
  (gethash id *dump-objects*))

(defun load-dump-objects (stream)
  (let ((hash (make-hash-table :test 'eql)))
    (loop for key = (read stream)
          for value = (read stream)
          while (not (eq key +dump-end-of-object-mark+))
          do (setf (gethash key hash) value))
    hash))

(defun load-db-hash (hash stream)
  (clrhash hash)
  (let ((*dump-objects* (load-dump-objects stream)))
    (loop for key = (read stream nil stream)
          for value = (read stream nil stream)
          while (not (eq key stream))
          do (setf (gethash key hash)
                   (case value
                     (+dump-zset-mark+ (load-zset stream))
                     (+dump-set-mark+  (load-set stream))
                     (+dump-hash-mark+ (load-hash stream))
                     (t                value))))))

(defun load-zset (stream)
  (let ((zset (make-zset)))
    (loop for key = (read stream)
          for score = (read stream)
          until (eq key +dump-end-of-object-mark+)
          do (zset-add zset score key))
    zset))

(defun load-set (stream)
  (let ((set (make-set)))
    (loop for value = (read stream)
          until (eq value +dump-end-of-object-mark+)
          do (set-add set value))
    set))

(defun load-hash (stream)
  (let ((hash (make-hash-table :test 'equalp)))
    (loop for key = (read stream)
          for value = (read stream)
          until (eq key +dump-end-of-object-mark+)
          do (setf (gethash key hash) value))
    hash))
