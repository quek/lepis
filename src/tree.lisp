(defpackage :lepis.tree
  (:use :cl :anaphora)
  (:export #:tree-add
           #:tree-delete
           #:tree-size
           #:tree-search
           #:tree-search-range-by-rank
           #:tree-search-range-by-score))

(in-package :lepis.tree)

#|
* 各節は赤または黒に区分される。
* ある節が赤ならばその子は必ず黒である。黒節の子に制限はない。
* ルートから葉までの黒節の個数を「黒高さ」という。赤黒木はどの経路でも黒高さが同じになる。
* ルートの節は黒である。
|#

(defconstant +rbt-red+ 0)
(defconstant +rbt-black+ 1)

(declaim (inline red-p))
(defun red-p (node)
  (and node (= (node-color node) +rbt-red+)))

(declaim (inline black-p))
(defun black-p (node)
  (not (red-p node)))

(defstruct node
  key
  value
  (color +rbt-red+)
  (size 1)
  left
  right)

(defun tree-size (tree)
  (if tree
      (node-size tree)
      0))

(defun rotate-right (node)
  (let* ((left (node-left node))
         (left-right (node-right left))
         (right (node-right node)))
    (setf (node-left node) left-right
          (node-right left) node
          (node-color left) (node-color node)
          (node-color node) +rbt-red+
          (node-size left) (node-size node)
          (node-size node)
          (+ (if left-right (node-size left-right) 0)
             (if right (node-size right) 0)
             1))
    left))

(defun rotate-left (node)
  (let* ((right (node-right node))
         (right-left (node-left right))
         (left (node-left node)))
    (setf (node-right node) right-left
          (node-left right) node
          (node-color right) (node-color node)
          (node-color node) +rbt-red+
          (node-size right) (node-size node)
          (node-size node)
          (+ (if right-left (node-size right-left) 0)
             (if left (node-size left) 0)
             1))
    right))

(defun tree-add (tree key value)
  (aprog1 (insert tree key value)
    (setf (node-color it) +rbt-black+)))

(defun insert (node key value)
  (let (flag)
    (cond ((null node)
           (values (make-node :key key :value value) nil))
          ((< key (node-key node))
           (setf (values (node-left node) flag)
                 (insert (node-left node) key value))
           (rbt-balance-insert-left node flag))
          ((> key (node-key node))
           (setf (values (node-right node) flag)
                 (insert (node-right node) key value))
           (rbt-balance-insert-right node flag))
          (t
           (push value (node-value node))
           (incf (node-size node))
           (values node t)))))

(defun rbt-split (node)
  (setf (node-color node) +rbt-red+
        (node-color (node-left node)) +rbt-black+
        (node-color (node-right node)) +rbt-black+))


(defun rbt-balance-insert-left (node flag)
  (cond (flag)
        ((black-p node)
         (setf flag t)
         (when (red-p (node-right (node-left node)))
           ;; 左(赤)の子に赤がある
           (setf (node-left node) (rotate-left (node-left node))))
         (when (red-p (node-left (node-left node)))
           (if (red-p (node-right node))
               ;; 赤が 2 つ続く
               (progn (rbt-split node)
                      (setf flag nil))
               (setf node (rotate-right node))))))
  (incf (node-size node))
  (values node flag))

(defun rbt-balance-insert-right (node flag)
  (cond (flag)
        ((black-p node)
         (setf flag t)
         (when (red-p (node-left (node-right node)))
           ;; 右(赤)の子に赤がある
           (setf (node-right node) (rotate-right (node-right node))))
         (when (red-p  (node-right (node-right node)))
           (if (red-p (node-left node))
               ;; 赤が 2 つ続く
               (progn
                 (rbt-split node)
                 (setf flag nil))
               (setf node (rotate-left node))))))
  (incf (node-size node))
  (values node flag))

(defun tree-search (node key)
  (if (null node)
      nil
      (let ((x (node-key node)))
        (cond ((= key x)
               node)
              ((< key x)
               (tree-search (node-left node) key))
              (t
               (tree-search (node-right node) key))))))


(defun tree-at-rank (node rank)
  (if (or (< rank 0) (<= (tree-size node) rank))
      nil
      (%tree-at-rank node rank)))

(defun %tree-at-rank (node rank)
  (let ((size (tree-size (node-left node))))
    (cond ((< rank size)
           (%tree-at-rank (node-left node) rank))
          ((= rank size)
           node)
          (t
           (%tree-at-rank (node-right node) (- rank size 1))))))

#+nil
(let (tree)
  (loop for i to 9 do (setf tree (tree-add tree i i)))
  (loop for i from -1 to 10 collect (node-key (or (tree-at-rank tree i) (make-node)))))
;;⇒ (NIL 0 1 2 3 4 5 6 7 8 9 NIL)


(defun map-tree (function tree &rest more-trees)
  (%map-tree function (cons tree more-trees)))

(defun %map-tree (function trees)
  (when (every #'identity trees)
      (%map-tree function (mapcar #'node-left trees))
      (apply function trees)
      (%map-tree function (mapcar #'node-right trees))))

#+nil
(let (x y)
  (loop for i to 9 do (setf x (tree-add x i i) y (tree-add y (+ i i) (+ i i))))
  (map-tree (lambda (&rest trees) (print (mapcar #'node-key trees))) x y))
;;→ 
;;   (0 0) 
;;   (1 2) 
;;   (2 4) 
;;   (3 6) 
;;   (4 8) 
;;   (5 10) 
;;   (6 12) 
;;   (7 14) 
;;   (8 16) 
;;   (9 18) 
;;⇒ NIL


(defun tree-search-range-by-rank (node start end)
  (let ((count (- end start -1)))
    (when (plusp count)
      (nreverse (%range-by-rank node start count nil)))))

(defun %range-by-rank (node start count parents)
  (let ((size (tree-size (node-left node))))
    (cond ((< start size)
           (%range-by-rank (node-left node) start count (cons node parents)))
          ((= start size)
           (let ((acc nil))
             (mapc (lambda (node)
                     #1=(if (zerop count)
                            (return-from %range-by-rank acc))
                     (push node acc)
                     (decf count)
                     #1#
                     (block map
                       (map-tree (lambda (node)
                                   #1#
                                   (push node acc)
                                   (decf count))
                                 (node-right node))))
                   (cons node parents))
             acc))
          (t
           (%range-by-rank (node-right node) (- start size 1) count parents)))))
#+nil
(let (node)
  (loop for i to 10 do (setf node (tree-add node i i)))
  (mapcar #'node-key (tree-search-range-by-rank node 3 6)))
;;⇒ (3 4 5 6)


(defun tree-search-range-by-score (node min max)
  (nreverse (%tree-search-range-by-score node min max nil)))

(defun %tree-search-range-by-score (node min max acc)
  (if (null node)
      acc
      (let ((key (node-key node)))
        (when (< min key)
          (setf acc (%tree-search-range-by-score (node-left node) min max acc)))
        (when (<= min key max)
          (setf acc (cons node acc)))
        (when (< key max)
          (setf acc (%tree-search-range-by-score (node-right node) min max acc)))
        acc)))

(defun search-min (node)
  (if (null (node-left node))
      node
      (search-min (node-left node))))

(defun tree-delete (node delete-key)
  (if (null node)
      (values nil t)
      (let (flag
            (key (node-key node))
            (left (node-left node))
            (right (node-right node)))
        (cond ((= delete-key key)
               (cond ((and (null left) (null right))
                      (values nil (red-p node)))
                     ((null right)
                      (setf (node-color left) +rbt-black+)
                      (values left t))
                     ((null left)
                      (setf (node-color right) +rbt-black+)
                      (values right t))
                     (t
                      (let ((min (search-min right)))
                        (setf (node-key node) (node-key min)
                              (node-value node) (node-value min)
                              (values (node-right node) flag)
                              (tree-delete right (node-key min)))
                        (balance-right node flag)))))
              ((< delete-key key)
               (setf (values (node-left node) flag)
                     (tree-delete left delete-key))
               (balance-left node flag))
              (t
               (setf (values (node-right node) flag)
                     (tree-delete right delete-key))
               (balance-right node flag))))))

(defun balance-left (node flag)
  (unless flag
    (let ((right (node-right node)))
      (cond ((and (black-p (node-left right))
                  (black-p (node-right right)))
             (if (black-p right)
                 (progn
                   (setf (node-color right) +rbt-red+)
                   (if (red-p node)
                       (setf (node-color node) +rbt-black+
                             flag t)
                       (setf flag nil)))
                 (setf node (rotate-left node)
                       (node-left node) (balance-left (node-left node) nil)
                       flag t)))
            (t
             (when (red-p (node-left right))
               (setf (node-right node) (rotate-right right)))
             (setf node (rotate-left node)
                   (node-color (node-left node)) +rbt-black+
                   (node-color (node-right node)) +rbt-black+
                   flag t)))))
  (when node
    (setf (node-size node)
          (+ (tree-size (node-left node))
             (tree-size (node-right node))
             1)))
  (values node flag))

(defun balance-right (node flag)
  (unless flag
    (let ((left (node-left node)))
      (cond ((and (black-p (node-left left))
                  (black-p (node-right left)))
             (if (black-p left)
                 (progn
                   (setf (node-color left) +rbt-red+)
                   (if (red-p node)
                       (setf (node-color node) +rbt-black+
                             flag t)
                       (setf flag nil)))
                 (setf node (rotate-right node)
                       (node-right node) (balance-right (node-right node) nil)
                       flag t)))
            (t
             (when (red-p (node-right left))
               (setf (node-left node) (rotate-left left)))
             (setf node (rotate-right node)
                   (node-color (node-right node)) +rbt-black+
                   (node-color (node-left node)) +rbt-black+
                   flag t)))))
  (when node
    (setf (node-size node)
          (+ (tree-size (node-left node))
             (tree-size (node-right node))
             1)))
  (values node flag))

(defun rbt-check (node)
  (cond ((null node)
         0)
        ((and (red-p node)
              (or (red-p (node-left node))
                  (red-p (node-right node))))
         (error "error1"))
        (t
         (let ((a (rbt-check (node-left node)))
               (b (rbt-check (node-right node))))
           (when (/= a b)
             (error "error2"))
           (if (red-p node)
               a
               (1+ a))))))


(defun rbt-print-node (node n)
  (when node
    (rbt-print-node (node-left node) (1+ n))
    (format t "~&")
    (dotimes (i n)
      (format t "    "))
    (format t "~:[R~;B~] ~a ~a~%" (black-p node) (node-key node) (node-value node))
    (rbt-print-node (node-right node) (1+ n))))

(defun shuffle (seq)
  (let ((n (length seq)))
    (dotimes (i n seq)
      (rotatef (elt seq i)
               (elt seq (+ i (random (- n i))))))))

#+nil
(loop repeat 100 do
 (let ((node nil))
   (loop for i in (shuffle (loop for i below 1000 collect i))
         do (setf node (tree-add node i i)))
   (assert (= 0 (node-value (tree-search node 0))))
   (assert (= 77 (node-value (tree-search node 77))))
   (assert (= 100 (node-value (tree-search node 100))))
   (tree-delete node 73)
   (tree-delete node 76)
   (assert (equal '(71 72 74 75 77)
                  (mapcar #'node-value (tree-search-range-by-score node 71 77))))
   (assert (= 998 (node-size node)))))
