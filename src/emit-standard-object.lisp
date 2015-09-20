(in-package :lepis)

(defmethod emit ((object standard-object) stream)
  (format stream "#.(let ((x (allocate-instance (find-class '~s))))" (type-of object))
  (let ((*sharp-dot* nil))
   (loop for slot in (sb-pcl:class-slots (class-of object)) do
     (format stream "~%(setf (slot-value x '~s)" (sb-pcl:slot-definition-name slot))
     (emit-object (slot-value object (sb-pcl:slot-definition-name slot)) stream)
     (write-char #\) stream)))
  (format stream "~%x)"))

