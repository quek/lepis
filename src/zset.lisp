(defpackage :lepis.zset
  (:use :cl :lepis.tree)
  (:export #:make-zset
           #:zset-p
           #:zset-card
           #:zset-add
           #:zset-delete
           #:zset-inc
           #:zset-interstore
           #:zset-rem
           #:zset-range
           #:zset-range-by-score
           #:zset-rank
           #:zset-score))

(in-package :lepis.zset)

(defstruct zset
  (hash (make-hash-table :test 'equalp))
  tree)

(defun zset-card (zset)
  (hash-table-count (zset-hash zset)))

(defun zset-add (zset &rest score-key-s)
  (let ((hash (zset-hash zset))
        (new-count 0))
    (loop for (score key) on score-key-s by #'cddr
          for old-score = (gethash key hash)
          if old-score
            do (setf (zset-tree zset) (tree-delete (zset-tree zset) old-score key))
          else
            do (incf new-count)
          do (setf (gethash key hash) score
                   (zset-tree zset) (tree-add (zset-tree zset) score key)))
    new-count))

(defun zset-inc (zset key delta)
  (let ((hash (zset-hash zset)))
    (let ((score (gethash key hash)))
      (if score
          (progn
            (setf (zset-tree zset) (tree-delete (zset-tree zset) score key))
            (incf score delta))
          (setf score delta))
      (setf (gethash key hash) score
            (zset-tree zset) (tree-add (zset-tree zset) score key))
      score)))

(defun zset-interstore (dest zset-weight-list aggregate)
  (let* ((zset-weight-list (sort zset-weight-list (lambda (a b)
                                                    (<= (zset-card (car a))
                                                        (zset-card (car b))))))
         (car (car zset-weight-list))
         (car-zset (car car))
         (car-weight (cdr car)))
    (lepis.tree::map-tree
     (lambda (node)
       (let* ((key (lepis.tree::node-value node))
              (score (* (zset-score car-zset key) car-weight)))
         (loop for (zset . weight) in (cdr zset-weight-list)
               for other-score = (zset-score zset key)
               do (if other-score
                      (setf score (funcall aggregate score (* other-score weight)))
                      (progn
                        (setf score nil)
                        (loop-finish))))
         (when score
           (zset-add dest score key))))
     (zset-tree car-zset))))

(defun zset-range (zset start stop with-scores from-end)
  (let ((tree (zset-tree zset)))
    (mapcar (if with-scores
                (lambda (node)
                  (cons (lepis.tree::node-value node)
                        (lepis.tree::node-key node)))
                #'lepis.tree::node-value)
            (tree-search-range-by-rank tree start stop from-end))))

(defun zset-range-by-score (zset min max with-scores offset limit)
  (let ((tree (zset-tree zset)))
    (mapcar (if with-scores
                (lambda (node)
                  (cons (lepis.tree::node-value node)
                        (lepis.tree::node-key node)))
                #'lepis.tree::node-value)
            (tree-search-range-by-score tree min max offset limit))))

(defun zset-rank (zset key)
  (let ((score (gethash key (zset-hash zset))))
    (when score
      (tree-rank (zset-tree zset) score key))))

(defun zset-delete (zset &rest keys)
  (loop with hash = (zset-hash zset)
        with tree = (zset-tree zset)
        for x in keys
        for score = (gethash x hash)
        if score
          sum (prog1 1
                (remhash x hash)
                (tree-delete tree score x))))

(defun zset-score (zset key)
  (gethash key (zset-hash zset)))

#+nil
(let ((zset (make-zset)))
  (zset-add zset 1 "a" 2 "b" 3 "c" 4 "d" 5 "f")
  (values (zset-range zset 1 2 nil)
          (zset-range zset 2 4 t)))
;;⇒ ("b" "c")
;;   (("c" . 3) ("d" . 4) ("f" . 5))

