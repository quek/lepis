(in-package :lepis)

(defconstant +dump-end-of-object-mark+ '+dump-end-of-object-mark+)
(defconstant +dump-zset-mark+ '+dump-zset-mark+)
(defconstant +dump-set-mark+ '+dump-set-mark+)
(defconstant +dump-hash-mark+ '+dump-hash-mark+)

(defvar *dump-objects* nil)
(defvar *set-refrence-functions* nil)

(defun make-object-table (hash)
  (let ((table (make-hash-table :test 'eql))
        (done (make-hash-table :test 'eql)))
    (maphash (lambda (key value)
               (store-object key table done)
               (store-value-object value table done))
             hash)
    table))

(defgeneric store-object (object hash done))

(defmethod store-object (object hash done)
  nil)

(defmethod store-object ((object cons) hash done)
  (sunless (gethash object done)
    (setf it t)
    (mapc (lambda (x) (store-object x hash done)) object)
    (setf (gethash object hash) (hash-table-count hash))))

(defmethod store-object ((object hash-table) hash done)
  (sunless (gethash object done)
    (setf it t)
    (maphash (lambda (key value)
               (store-object key hash done)
               (store-object value hash done))
             object)
    (setf (gethash object hash) (hash-table-count hash))))

(defmethod store-object ((object sb-pcl::slot-object) hash done)
  (sunless (gethash object done)
    (setf it t)
    (store-slots object hash done)
    (setf (gethash object hash) (hash-table-count hash))))

(defun store-slots (object hash done)
  (loop for slot in (sb-mop:class-slots (class-of object))
        for slot-name = (sb-mop:slot-definition-name slot)
        if (slot-boundp object slot-name)
          do (store-object (slot-value object slot-name)
                           hash done)))

(defmacro with-value-type-case (var &key zset set hash else)
  `(cond ((lepis.zset:zset-p ,var)
          ,zset)
         ((lepis.set:set-p ,var)
          ,set)
         ((hash-table-p ,var)
          ,hash)
         (t
          ,else)))

(defun store-value-object (object hash done)
  (with-value-type-case object
    :zset (maphash (lambda (key value)
                     (declare (ignore value))
                     (store-object key hash done))
                   (lepis.zset::zset-hash object))
    :set (lepis.set:map-set (lambda (value)
                              (store-object value hash done))
                object)
    :hash (maphash (lambda (key value)
                     (store-object key hash done)
                     (store-object value hash done))
                   object)
    :else (store-object object hash done)))


(defgeneric emit (object stream sharp-dot))

(defmethod emit (object stream sharp-dot)
  (write-char #\space stream)
  (prin1 object stream))

(defmethod emit ((object null) stream sharp-dot)
  (write-char #\space stream)
  (prin1 nil stream))

(defmethod emit ((list cons) stream sharp-dot)
  (format stream "~%~a(let ((x (list" sharp-dot)
  (let ((setter
          (loop for i in list
                for n from 0
                for setter = (format nil "(setf (nth ~d x) ~~a)" n)
                collect (emit-object i stream :setter setter))))
    (format stream ")))~{ ~a~} x)" setter)))

(defmethod emit ((object string) stream sharp-dot)
  (print object stream))

(defmethod emit ((array array) stream sharp-dot)
  ;; FIXME :initial-contents 使うと遅いので実装をわけているがもうちょっとなんとかしたい
  (if (loop for i across array
            always (numberp i))
      (progn
        (format stream "~%~a(let ((x #(" sharp-dot)
        (let ((setter
                (loop for x across array
                      for n from 0
                      for setter = (format nil "(setf (aref x ~d) ~~a)" n)
                      collect (emit-object x stream :setter setter))))
          (format stream ")))~%~{ ~a~} (coerce x '~a))" setter (type-of array))))
      (progn
        (format stream "~%~a(let ((x (make-array ~d :initial-contents (list"
                sharp-dot (array-dimension array 0))
        (let ((setter
                (loop for x across array
                      for n from 0
                      for setter = (format nil "(setf (aref x ~d) ~~a)" n)
                      collect (emit-object x stream :setter setter))))
          (format stream "))))~%~{ ~a~} (coerce x '~a))" setter (type-of array))))))

(defun emit-object (object stream &key setter (sharp-dot ""))
  (aif (gethash object *dump-objects*)
       (progn
         (terpri stream)
         (if setter
             (progn
               (write-string " t" stream)
               (format nil "(LEPIS::M ~a)"
                       (format nil setter
                               (format nil "(~s ~d)" 'l it))))
             (progn
               (format stream " ~a(~s ~d)" sharp-dot 'l it)
               "")))
       (progn
         (emit object stream sharp-dot)
         "")))

(defmacro m (&body body)
  `(push (lambda () ,@body) *set-refrence-functions*))

(defun emit-value-object (object stream)
  (with-value-type-case object
    :zset (progn
            (emit +dump-zset-mark+ stream "")
            (maphash (lambda (key score)
                       (emit-object key stream :sharp-dot " #.")
                       (emit-object score stream :sharp-dot " #."))
                     (lepis.zset::zset-hash object))
            (emit +dump-end-of-object-mark+ stream "")
            (emit +dump-end-of-object-mark+ stream ""))
    :set (progn
           (emit +dump-set-mark+ stream "")
           (lepis.set:map-set (lambda (value)
                                (emit-object value stream :sharp-dot " #."))
                  object)
           (emit +dump-end-of-object-mark+ stream ""))
    :hash (progn
            (emit +dump-hash-mark+ stream "")
            (maphash (lambda (key value)
                       (emit-object key stream :sharp-dot " #.")
                       (emit-object value stream :sharp-dot " #."))
                     object)
            (emit +dump-end-of-object-mark+ stream "")
            (emit +dump-end-of-object-mark+ stream ""))
    :else (emit-object object stream :sharp-dot " #.")))

(defun dump-db-hash (hash expire-hash stream)
  (let ((*dump-objects* (make-object-table hash)))
    (maphash (lambda (key value)
               (terpri stream)
               (emit value stream "")
               (emit key stream " #."))
             *dump-objects*)
    (emit +dump-end-of-object-mark+ stream "") ;for key
    (emit +dump-end-of-object-mark+ stream "") ;for value
    (maphash (lambda (key value)
               (emit-object key stream :sharp-dot " #.")
               (emit-value-object value stream))
             hash)
    (emit +dump-end-of-object-mark+ stream "") ;for key
    (emit +dump-end-of-object-mark+ stream "") ;for value
    (maphash (lambda (key value)
               (emit-object key stream  :sharp-dot " #.")
               (emit-object value stream :sharp-dot " #."))
             expire-hash)
    (emit +dump-end-of-object-mark+ stream "")   ;for key
    (emit +dump-end-of-object-mark+ stream ""))) ;for value

(defun l (id)
  (gethash id *dump-objects*))

(defun load-dump-objects (stream)
  (let ((*dump-objects* (make-hash-table :test 'eql))
        (*set-refrence-functions* nil))
    (loop for key = (read stream)
          for value = (read stream)
          while (not (eq key +dump-end-of-object-mark+))
          do (setf (gethash key *dump-objects*) value))
    (loop for f in *set-refrence-functions*
          do (funcall f))
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
