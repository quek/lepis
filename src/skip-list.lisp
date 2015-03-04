(defpackage :lepis.skip-list
  (:use :cl :anaphora :lepis.util)
  (:export #:make-skip-list
           #:skip-list-add
           #:skip-list-search
           #:skip-list-remove
           #:map-skip-list))

(in-package :lepis.skip-list)

(defstruct node
  (value nil)
  (next #() :type simple-vector))

(defstruct (skip-list (:constructor %make-skip-list))
  (level-p 0.25)
  (max-level 0 :type fixnum)
  head)

(defun make-skip-list (&key (expected-record-count #xffffffffffffffff) (p 0.25))
  (let ((max-level (ceiling (log expected-record-count (/ 1 p)))))
   (%make-skip-list :level-p p
                    :max-level max-level
                    :head (make-node :next (make-array max-level :initial-element nil)))))

(defun %skip-list-search (skip-list value)
  (declare (optimize (speed 3) (safety 0)))
  (loop with max-level fixnum = (skip-list-max-level skip-list)
        with level fixnum = (1- max-level)
        with node-1 = (skip-list-head skip-list)
        with prevs = (make-array max-level :initial-element nil)
        for node = (aref (node-next node-1) level)
          then (aref (node-next node-1) level)
        if node
          do (let ((node-value (node-value node)))
               (cond ((value< node-value value)
                      (setf node-1 node))
                     ((value< value node-value)
                      #1=(progn
                           (setf (aref prevs level) node-1)
                           (when (= -1 (decf level))
                             (return (values nil prevs)))))
                     (t
                      (setf (aref prevs level) node-1)
                      (return (values node prevs)))))
        else
          do #1#))

(defun skip-list-search (skip-list value)
  (let ((node (%skip-list-search skip-list value)))
    (if node
        (values (node-value node) t)
        (values nil nil))))

(defun compute-level-for-add (skip-list)
  (let ((p (skip-list-level-p skip-list))
        (max-level (skip-list-max-level skip-list)))
    (loop for i from 1
          if (= i max-level)
            do (return max-level)
          if (< p (random 1.0))
            do (return i))))

(defun %skip-list-add (skip-list prevs value)
  (let* ((top-level (compute-level-for-add skip-list))
         (new-node (make-node :value value
                              :next (make-array top-level :initial-element nil))))
    (loop for level fixnum from 0 below top-level
          for prev across (the simple-vector prevs)
          do (shiftf (aref (the simple-vector (node-next new-node)) level)
                     (aref (the simple-vector (node-next prev)) level)
                     new-node))))

(defun skip-list-add (skip-list value)
  (multiple-value-bind (node prevs) (%skip-list-search skip-list value)
    (if node
        nil
        (progn
          (%skip-list-add skip-list prevs value)
          t))))

;;     3     6
;; 1   3   5 6
;; 1 2 3 4 5 6
(defun %skip-list-remove (node prevs)
  (loop for level from 0
        for prev across prevs
        if prev
          do (loop for i from level downto 0
                   do (loop until (eq node (aref (node-next prev) i))
                            do (setf prev (aref (node-next prev) i))
                            finally (setf (aref (node-next prev) i)
                                          (aref (node-next node) i))))
             (return t)))

(defun skip-list-remove (skip-list value)
  (multiple-value-bind (node prevs) (%skip-list-search skip-list value)
    (when node
      (%skip-list-remove node prevs)
      node)))


(defun map-skip-list (function skip-list)
  (loop for node = (aref (node-next (skip-list-head skip-list)) 0)
          then (aref (node-next node) 0)
        while node
        do (funcall function (node-value node))))


(defun print-skip-list (skip-list &optional (stream *standard-output*))
  (loop for level from (1- (skip-list-max-level skip-list)) downto 0
        do (format stream "~&~d ~d" level
                   (loop for node = (aref (node-next (skip-list-head skip-list))
                                          level)
                           then (aref (node-next node) level)
                         while node
                         sum 1)))
  #+nil
  (loop with max-level fixnum = (skip-list-max-level skip-list)
        with level fixnum = (1- max-level)
          initially (format stream "~&~a " level)
        for node = (aref (node-next (skip-list-head skip-list)) level)
          then (aref (node-next node) level)
        if node
          do (let ((key (node-key node))
                   (value (node-val node)))
               (format stream "~a:~a " key value))
        else
          do (if (= -1 (decf level))
                 (loop-finish)
                 (progn
                   (format stream "~%~a " level)
                   (setf node (skip-list-head skip-list))))))

(defun %%%skip-list-test ()
  (let ((s (make-skip-list)))
    (skip-list-add s "a")
    (skip-list-add s "c")
    (skip-list-add s "d")
    (skip-list-add s "b")
    (assert (string= "b" (skip-list-search s "b")))
    (skip-list-remove s "b")
    (assert (null (skip-list-search s "b")))
    (loop repeat 1000000
          for value = (random 100000000000)
          do (skip-list-add s value))
    (print-skip-list s)))

(aprog1 (make-skip-list)
  (skip-list-add it 3)
  (skip-list-add it 1)
  (skip-list-add it 2))
