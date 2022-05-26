(in-package :lepis)

(defmethod emit ((object standard-object) stream sharp-dot)
  (format stream "~%~a(let ((x (allocate-instance (find-class '~s))))" sharp-dot (type-of object))
  (let ((*sharp-dot* nil))
    (let ((setter 
            (loop for slot-definiiton in (sb-mop:class-slots (class-of object))
                  for slot = (sb-mop:slot-definition-name slot-definiiton)
                  if (slot-boundp object slot)
                    collect (prog2
                                (format stream "~%(setf (slot-value x '~s)" slot)
                                (emit-object
                                 (slot-value object slot) stream
                                 :setter
                                 (format nil "(lambda () (setf (slot-value x '~s) ~~a))" slot))
                              (write-char #\) stream)))))
      (format stream "~% ~{~a~} x)" setter))))

