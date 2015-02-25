(defpackage :lepis.zset
  (:use :cl :lepis.tree)
  (:export #:zset-add
           #:zset-rem
           #:zset-range))

(in-package :lepis.zset)

(defstruct zset
  (hash (make-hash-table :test 'equal))
  tree)

(defun zset-add (zset &rest score-key-s)
  (let ((hash (zset-hash zset))
        (new-count 0))
    (loop for (score key) on score-key-s by #'cddr
          unless (gethash key hash)
            do (incf new-count)
          do (setf (gethash key hash) score
                   (zset-tree zset) (tree-add (zset-tree zset) score key)))
    new-count))


#+nil
(let ((zset (make-zset)))
  (zset-add zset 1 "a" 2 "b")
  zset)
