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

(defgeneric store-object (object hash))

(defmethod store-object (object hash)
  nil)

(defmethod store-object ((object cons) hash)
  (sunless (gethash object hash)
    (mapc (lambda (x) (store-object x hash)) object)
    (setf it (hash-table-count hash))))

(defmethod store-object ((object hash-table) hash)
  (sunless (gethash object hash)
    (maphash (lambda (key value)
               (store-object key hash)
               (store-object value hash))
             object)
    (setf it (hash-table-count hash))))

(defmethod store-object ((object sb-pcl::slot-object) hash)
  (sunless (gethash object hash)
    (store-slots object hash)
    (setf it (hash-table-count hash))))

(defun store-slots (object hash)
  (loop for slot in (sb-pcl:class-slots (class-of object))
        for slot-name = (sb-pcl:slot-definition-name slot)
        if (slot-boundp object slot-name)
          do (store-object (slot-value object slot-name)
                           hash)))

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


(defgeneric emit (object stream))

(defmethod emit (object stream)
  (print object stream))

(defmethod emit ((list cons) stream)
  (format stream "~%#.(list ")
  (loop for i in list
        do (emit-object i stream))
  (write-char #\) stream))

(defvar *sharp-dot* t)

(defun emit-object (object stream)
  (aif (gethash object *dump-objects*)
       (progn
         (terpri stream)
         (when *sharp-dot*
           (write-string "#." stream))
         (format stream "(~s ~d)" 'l it))
       (emit object stream)))

(defun emit-value-object (object stream)
  (with-value-type-case object
    :zset (progn
            (emit +dump-zset-mark+ stream)
            (maphash (lambda (key score)
                       (emit-object key stream)
                       (emit-object score stream))
                     (lepis.zset::zset-hash object))
            (emit +dump-end-of-object-mark+ stream)
            (emit +dump-end-of-object-mark+ stream))
    :set (progn
           (emit +dump-set-mark+ stream)
           (lepis.set:map-set (lambda (value)
                                (emit-object value stream))
                  object)
           (emit +dump-end-of-object-mark+ stream))
    :hash (progn
            (emit +dump-hash-mark+ stream)
            (maphash (lambda (key value)
                       (emit-object key stream)
                       (emit-object value stream))
                     object)
            (emit +dump-end-of-object-mark+ stream)
            (emit +dump-end-of-object-mark+ stream))
    :else (emit-object object stream)))

(defun dump-db-hash (hash expire-hash stream)
  (let ((*dump-objects* (make-object-table hash)))
    (maphash (lambda (key value)
               (emit value stream)
               (emit key stream))
             *dump-objects*)
    (emit +dump-end-of-object-mark+ stream) ;for key
    (emit +dump-end-of-object-mark+ stream) ;for value
    (maphash (lambda (key value)
               (emit-object key stream)
               (emit-value-object value stream))
             hash)
    (emit +dump-end-of-object-mark+ stream) ;for key
    (emit +dump-end-of-object-mark+ stream) ;for value
    (maphash (lambda (key value)
               (emit-object key stream)
               (emit-object value stream))
             expire-hash)
    (emit +dump-end-of-object-mark+ stream)   ;for key
    (emit +dump-end-of-object-mark+ stream))) ;for value

(defun l (id)
  (gethash id *dump-objects*))

(defun load-dump-objects (stream)
  (let ((*dump-objects* (make-hash-table :test 'eql)))
    (loop for key = (read stream)
          for value = (read stream)
          while (not (eq key +dump-end-of-object-mark+))
          do (setf (gethash key *dump-objects*) value))
    *dump-objects*))

(defun load-db-hash (hash expire-hash stream)
  (clrhash hash)
  (let ((*dump-objects* (load-dump-objects stream)))
    (loop for key = (read stream)
          for value = (read stream)
          while (not (eq key +dump-end-of-object-mark+))
          do (setf (gethash key hash)
                   (case value
                     (+dump-zset-mark+ (load-zset stream))
                     (+dump-set-mark+  (load-set stream))
                     (+dump-hash-mark+ (load-hash stream))
                     (t                value))))
    (loop for key = (read stream)
          for value = (read stream)
          while (not (eq key +dump-end-of-object-mark+))
          do (setf (gethash key expire-hash) value))))

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
