(in-package :lepis)

(defmethod emit ((object standard-object) stream)
  (format stream "~%#.(let ((x (allocate-instance (find-class '~s))))" (type-of object))
  (let ((*sharp-dot* nil))
    (loop for slot-definiiton in (sb-mop:class-slots (class-of object))
          for slot = (sb-mop:slot-definition-name slot-definiiton)
          if (slot-boundp object slot)
            do
               (format stream "~%(setf (slot-value x '~s)" slot)
               (emit-object (slot-value object slot) stream)
               (write-char #\) stream)))
  (format stream "~%x)"))

