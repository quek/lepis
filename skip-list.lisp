(in-package :lepis)

(defstruct skip-list-node
  (key 0.0d0 :type double-float)
  (val nil)
  (next #() :type simple-vector))

(defstruct (skip-list (:constructor %make-skip-list))
  (level-p 0.25)
  (max-level 0 :type fixnum)
  head)

(defun make-skip-list (&key (expected-record-count 100) (p 0.25))
  (let ((max-level (ceiling (log expected-record-count (/ 1 p)))))
   (%make-skip-list :level-p p
                    :max-level max-level
                    :head (make-skip-list-node :next (make-array max-level :initial-element nil)))))

(defun %skip-list-search (skip-list key)
  #+nil
  (declare (optimize (speed 3) (safety 0))
           (double-float key))
  (loop with max-level fixnum = (skip-list-max-level skip-list)
        with level fixnum = (1- max-level)
        with node-1 = (skip-list-head skip-list)
        with prevs = (make-array max-level :initial-element nil)
        for node = (aref (skip-list-node-next node-1) level)
          then (aref (skip-list-node-next node-1) level)
        if node
          do (let ((node-key (skip-list-node-key node)))
               (cond ((< node-key key)
                      (setf node-1 node))
                     ((< key node-key)
                      #1=(progn
                           (setf (aref prevs level) node-1)
                           (when (= -1 (decf level))
                             (return (values nil prevs)))))
                     (t
                      (setf (aref prevs level) node-1)
                      (return (values node prevs)))))
        else
          do #1#))

(defun skip-list-search (skip-list key)
  (let ((node (%skip-list-search skip-list key)))
    (if node
        (values (skip-list-node-val node) t)
        (values nil nil))))

(defun compute-level-for-add (skip-list)
  (let ((p (skip-list-level-p skip-list))
        (max-level (skip-list-max-level skip-list)))
    (loop for i from 1
          if (= i max-level)
            do (return max-level)
          if (< p (random 1.0))
            do (return i))))

(defun %skip-list-add (skip-list prevs key val)
  (let ((new-node (make-skip-list-node
                   :key key :val val
                   :next (make-array (the fixnum (skip-list-max-level skip-list))
                                     :initial-element nil)))
        (top-level (compute-level-for-add skip-list)))
    (loop for level fixnum from 0 below top-level
          for prev across (the simple-vector prevs)
          do (shiftf (aref (the simple-vector (skip-list-node-next new-node)) level)
                     (aref (the simple-vector (skip-list-node-next prev)) level)
                     new-node))))

(defun skip-list-add (skip-list key val)
  (let ((key (coerce key 'double-float)))
    (multiple-value-bind (node prevs) (%skip-list-search skip-list key)
      (if node
          (setf (skip-list-node-val node) val)
          (%skip-list-add skip-list prevs key val)))))

;;     3     6
;; 1   3   5 6
;; 1 2 3 4 5 6
(defun %skip-list-remove (node prevs)
  (loop for level from 0
        for prev across prevs
        if prev
          do (loop for i from level downto 0
                   do (loop until (eq node (aref (skip-list-node-next prev) i))
                            do (setf prev (aref (skip-list-node-next prev) i))
                            finally (setf (aref (skip-list-node-next prev) i)
                                          (aref (skip-list-node-next node) i))))
             (return t)))

(defun skip-list-remove (skip-list key)
  (multiple-value-bind (node prevs) (%skip-list-search skip-list key)
    (when node
      (%skip-list-remove node prevs)
      node)))



(defun print-skip-list (skip-list &optional (stream *standard-output*))
  (loop with max-level fixnum = (skip-list-max-level skip-list)
        with level fixnum = (1- max-level)
          initially (format stream "~&~a " level)
        for node = (aref (skip-list-node-next (skip-list-head skip-list)) level)
          then (aref (skip-list-node-next node) level)
        if node
          do (let ((key (skip-list-node-key node))
                   (value (skip-list-node-val node)))
               (format stream "~a:~a " key value))
        else
          do (if (= -1 (decf level))
                 (loop-finish)
                 (progn
                   (format stream "~%~a " level)
                   (setf node (skip-list-head skip-list))))))

(defun %%%skip-list-test ()
  (let ((s (make-skip-list)))
    (skip-list-add s 1 "a")
    (skip-list-add s 3 "c")
    (skip-list-add s 4 "d")
    (skip-list-add s 2 "b")
    (assert (string= "b" (skip-list-search s 2)))
    (skip-list-remove s 2)
    (assert (null (skip-list-search s 2)))
    (loop repeat 10
          for key = (random 1000)
          do (skip-list-add s key key))
    (print-skip-list s)))

(make-skip-list)
;;⇒ #S(SKIP-LIST
;;      :LEVEL-P 0.25
;;      :MAX-LEVEL 4
;;      :HEAD #S(SKIP-LIST-NODE :KEY 0.0d0 :VAL NIL :NEXT #(NIL NIL NIL NIL)))


