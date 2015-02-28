(defpackage :lepis.zset
  (:use :cl :lepis.tree)
  (:export #:make-zset
           #:zset-add
           #:zset-rem
           #:zset-range
           #:zset-range-by-score))

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

(defun zset-range (zset start stop with-scores)
  (let ((tree (zset-tree zset)))
    (mapcar (if with-scores
                (lambda (node)
                  (cons (lepis.tree::node-value node)
                        (lepis.tree::node-key node)))
                #'lepis.tree::node-value)
            (tree-search-range-by-rank tree start stop))))

(defun zset-range-by-score (zset min max with-scores)
  (let ((tree (zset-tree zset)))
    (mapcar (if with-scores
                (lambda (node)
                  (cons (lepis.tree::node-value node)
                        (lepis.tree::node-key node)))
                #'lepis.tree::node-value)
            (tree-search-range-by-score tree min max))))


#+nil
(let ((zset (make-zset)))
  (zset-add zset 1 "a" 2 "b" 3 "c" 4 "d" 5 "f")
  (values (zset-range zset 1 2 nil)
          (zset-range zset 2 4 t)))
;;⇒ ("b" "c")
;;   (("c" . 3) ("d" . 4) ("f" . 5))

