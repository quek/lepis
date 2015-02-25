(defpackage :lepis.tree
  (:use :cl :anaphora)
  (:export #:tree-add
           #:tree-delete
           #:tree-search
           #:tree-search-range))

(in-package :lepis.tree)

(defun tree-add (tree key value)
  (aprog1 (insert tree key value)
    (setf (node-color it) +rbt-black+)))


(defconstant +rbt-red+ 0)
(defconstant +rbt-black+ 1)

(declaim (inline red-p))
(defun red-p (node)
  (and node (= (node-color node) +rbt-red+)))

(declaim (inline black-p))
(defun black-p (node)
  (or (null node) (= (node-color node) +rbt-black+)))

(defstruct node
  key
  value
  (color +rbt-red+)
  left
  right)

(defun rotate-right (node)
  (let ((left (node-left node)))
    (setf (node-left node) (node-right left)
          (node-right left) node
          (node-color left) (node-color node)
          (node-color node) +rbt-red+)
    left))

(defun rotate-left (node)
  (let ((right (node-right node)))
    (setf (node-right node) (node-left right)
          (node-left right) node
          (node-color right) (node-color node)
          (node-color node) +rbt-red+)
    right))

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

(defun tree-search-range (node start end)
  (nreverse (%tree-search-range node start end nil)))

(defun %tree-search-range (node start end acc)
  (if (null node)
      acc
      (let ((key (node-key node)))
        (when (< start key)
          (setf acc (%tree-search-range (node-left node) start end acc)))
        (when (<= start key end)
          (setf acc (cons node acc)))
        (when (< key end)
          (setf acc (%tree-search-range (node-right node) start end acc)))
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
  (cond (flag (values node flag))
        ((and (black-p (node-left (node-right node)))
              (black-p (node-right (node-right node))))
         (if (black-p (node-right node))
             (progn
               (setf (node-color (node-right node)) +rbt-red+)
               (if (red-p node)
                   (progn
                     (setf (node-color node) +rbt-black+)
                     (values node t))
                   (values node nil)))
             (progn
               (setf node (rotate-left node)
                     (node-left node) (balance-left (node-left node) nil))
               (values node t))))
        (t
         (when (red-p (node-left (node-right node)))
           (setf (node-right node) (rotate-right (node-right node))))
         (setf node (rotate-left node)
               (node-color (node-left node)) +rbt-black+
               (node-color (node-right node)) +rbt-black+)
         (values node t))))

(defun balance-right (node flag)
  (cond (flag (values node flag))
        ((and (black-p (node-left (node-left node)))
              (black-p (node-right (node-left node))))
         (if (black-p (node-left node))
             (progn
               (setf (node-color (node-left node)) +rbt-red+)
               (if (red-p node)
                   (progn
                     (setf (node-color node) +rbt-black+)
                     (values node t))
                   (values node nil)))
             (progn
               (setf node (rotate-right node)
                     (node-right node) (balance-right (node-right node) nil))
               (values node t))))
        (t
         (when (red-p (node-right (node-left node)))
           (setf (node-left node) (rotate-left (node-left node))))
         (setf node (rotate-right node)
               (node-color (node-right node)) +rbt-black+
               (node-color (node-left node)) +rbt-black+)
         (values node t))))

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
   (loop for i in (shuffle (loop for i to 1000 collect i))
         do (setf node (insert node i i)
                  (node-color node) +rbt-black+))
   (assert (= 0 (node-value (tree-search node 0))))
   (assert (= 77 (node-value (tree-search node 77))))
   (assert (= 100 (node-value (tree-search node 100))))
   (tree-delete node 73)
   (tree-delete node 76)
   (assert (equal '(71 72 74 75 77)
                  (mapcar #'node-value (tree-search-range node 71 77))))))