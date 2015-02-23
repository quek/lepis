(in-package :lepis)

(defconstant +rbt-red+ 0)
(defconstant +rbt-black+ 1)

(declaim (inline rbt-red-p))
(defun rbt-red-p (node)
  (and node (= (rbt-node-color node) +rbt-red+)))

(declaim (inline rbt-black-p))
(defun rbt-black-p (node)
  (or (null node) (= (rbt-node-color node) +rbt-black+)))

(defstruct rbt-node
  key
  value
  (color +rbt-red+)
  left
  right)

(defun rbt-rotate-right (node)
  (let ((left (rbt-node-left node)))
    (setf (rbt-node-left node) (rbt-node-right left)
          (rbt-node-right left) node
          (rbt-node-color left) (rbt-node-color node)
          (rbt-node-color node) +rbt-red+)
    left))

(defun rbt-rotate-left (node)
  (let ((right (rbt-node-right node)))
    (setf (rbt-node-right node) (rbt-node-left right)
          (rbt-node-left right) node
          (rbt-node-color right) (rbt-node-color node)
          (rbt-node-color node) +rbt-red+)
    right))

(defun rbt-insert (node key value)
  (let (flag)
    (cond ((null node)
           (values (make-rbt-node :key key :value value) nil))
          ((< key (rbt-node-key node))
           (setf (values (rbt-node-left node) flag)
                 (rbt-insert (rbt-node-left node) key value))
           (rbt-balance-insert-left node flag))
          ((> key (rbt-node-key node))
           (setf (values (rbt-node-right node) flag)
                 (rbt-insert (rbt-node-right node) key value))
           (rbt-balance-insert-right node flag))
          (t
           (push value (rbt-node-value node))
           (values node t)))))

(defun rbt-split (node)
  (setf (rbt-node-color node) +rbt-red+
        (rbt-node-color (rbt-node-left node)) +rbt-black+
        (rbt-node-color (rbt-node-right node)) +rbt-black+))


(defun rbt-balance-insert-left (node flag)
  (cond (flag)
        ((rbt-black-p node)
         (setf flag t)
         (when (rbt-red-p (rbt-node-right (rbt-node-left node)))
           ;; 左(赤)の子に赤がある
           (setf (rbt-node-left node) (rbt-rotate-left (rbt-node-left node))))
         (when (rbt-red-p (rbt-node-left (rbt-node-left node)))
           (if (rbt-red-p (rbt-node-right node))
               ;; 赤が 2 つ続く
               (progn (rbt-split node)
                      (setf flag nil))
               (setf node (rbt-rotate-right node))))))
  (values node flag))

(defun rbt-balance-insert-right (node flag)
  (cond (flag)
        ((rbt-black-p node)
         (setf flag t)
         (when (rbt-red-p (rbt-node-left (rbt-node-right node)))
           ;; 右(赤)の子に赤がある
           (setf (rbt-node-right node) (rbt-rotate-right (rbt-node-right node))))
         (when (rbt-red-p  (rbt-node-right (rbt-node-right node)))
           (if (rbt-red-p (rbt-node-left node))
               ;; 赤が 2 つ続く
               (progn
                 (rbt-split node)
                 (setf flag nil))
               (setf node (rbt-rotate-left node))))))
  (values node flag))

(defun rbt-search (node key)
  (if (null node)
      nil
      (let ((x (rbt-node-key node)))
        (cond ((= key x)
               node)
              ((< key x)
               (rbt-search (rbt-node-left node) key))
              (t
               (rbt-search (rbt-node-right node) key))))))

(defun rbt-search-range (node start end)
  (nreverse (%rbt-search-range node start end nil)))

(defun %rbt-search-range (node start end acc)
  (if (null node)
      acc
      (let ((key (rbt-node-key node)))
        (when (< start key)
          (setf acc (%rbt-search-range (rbt-node-left node) start end acc)))
        (when (<= start key end)
          (setf acc (cons node acc)))
        (when (< key end)
          (setf acc (%rbt-search-range (rbt-node-right node) start end acc)))
        acc)))

(defun search-min (node)
  (if (null (rbt-node-left node))
      node
      (search-min (rbt-node-left node))))

(defun rbt-delete (node delete-key)
  (if (null node)
      (values nil t)
      (let (flag
            (key (rbt-node-key node))
            (left (rbt-node-left node))
            (right (rbt-node-right node)))
        (cond ((= delete-key key)
               (cond ((and (null left) (null right))
                      (values nil (rbt-red-p node)))
                     ((null right)
                      (setf (rbt-node-color left) +rbt-black+)
                      (values left t))
                     ((null left)
                      (setf (rbt-node-color right) +rbt-black+)
                      (values right t))
                     (t
                      (let ((min (search-min right)))
                        (setf (rbt-node-key node) (rbt-node-key min)
                              (rbt-node-value node) (rbt-node-value min)
                              (values (rbt-node-right node) flag)
                              (rbt-delete right (rbt-node-key min)))
                        (balance-right node flag)))))
              ((< delete-key key)
               (setf (values (rbt-node-left node) flag)
                     (rbt-delete left delete-key))
               (balance-left node flag))
              (t
               (setf (values (rbt-node-right node) flag)
                     (rbt-delete right delete-key))
               (balance-right node flag))))))

(defun balance-left (node flag)
  (cond (flag (values node flag))
        ((and (rbt-black-p (rbt-node-left (rbt-node-right node)))
              (rbt-black-p (rbt-node-right (rbt-node-right node))))
         (if (rbt-black-p (rbt-node-right node))
             (progn
               (setf (rbt-node-color (rbt-node-right node)) +rbt-red+)
               (if (rbt-red-p node)
                   (progn
                     (setf (rbt-node-color node) +rbt-black+)
                     (values node t))
                   (values node nil)))
             (progn
               (setf node (rbt-rotate-left node)
                     (rbt-node-left node) (balance-left (rbt-node-left node) nil))
               (values node t))))
        (t
         (when (rbt-red-p (rbt-node-left (rbt-node-right node)))
           (setf (rbt-node-right node) (rbt-rotate-right (rbt-node-right node))))
         (setf node (rbt-rotate-left node)
               (rbt-node-color (rbt-node-left node)) +rbt-black+
               (rbt-node-color (rbt-node-right node)) +rbt-black+)
         (values node t))))

(defun balance-right (node flag)
  (cond (flag (values node flag))
        ((and (rbt-black-p (rbt-node-left (rbt-node-left node)))
              (rbt-black-p (rbt-node-right (rbt-node-left node))))
         (if (rbt-black-p (rbt-node-left node))
             (progn
               (setf (rbt-node-color (rbt-node-left node)) +rbt-red+)
               (if (rbt-red-p node)
                   (progn
                     (setf (rbt-node-color node) +rbt-black+)
                     (values node t))
                   (values node nil)))
             (progn
               (setf node (rbt-rotate-right node)
                     (rbt-node-right node) (balance-right (rbt-node-right node) nil))
               (values node t))))
        (t
         (when (rbt-red-p (rbt-node-right (rbt-node-left node)))
           (setf (rbt-node-left node) (rbt-rotate-left (rbt-node-left node))))
         (setf node (rbt-rotate-right node)
               (rbt-node-color (rbt-node-right node)) +rbt-black+
               (rbt-node-color (rbt-node-left node)) +rbt-black+)
         (values node t))))

(defun rbt-check (node)
  (cond ((null node)
         0)
        ((and (rbt-red-p node)
              (or (rbt-red-p (rbt-node-left node))
                  (rbt-red-p (rbt-node-right node))))
         (error "error1"))
        (t
         (let ((a (rbt-check (rbt-node-left node)))
               (b (rbt-check (rbt-node-right node))))
           (when (/= a b)
             (error "error2"))
           (if (rbt-red-p node)
               a
               (1+ a))))))


(defun rbt-print-node (node n)
  (when node
    (rbt-print-node (rbt-node-left node) (1+ n))
    (format t "~&")
    (dotimes (i n)
      (format t "    "))
    (format t "~:[R~;B~] ~a ~a~%" (rbt-black-p node) (rbt-node-key node) (rbt-node-value node))
    (rbt-print-node (rbt-node-right node) (1+ n))))

(defun shuffle (seq)
  (let ((n (length seq)))
    (dotimes (i n seq)
      (rotatef (elt seq i)
               (elt seq (+ i (random (- n i))))))))

#+nil
(loop repeat 100 do
 (let ((node nil))
   (loop for i in (shuffle (loop for i to 1000 collect i))
         do (setf node (rbt-insert node i i)
                  (rbt-node-color node) +rbt-black+))
   (assert (= 0 (rbt-node-value (rbt-search node 0))))
   (assert (= 77 (rbt-node-value (rbt-search node 77))))
   (assert (= 100 (rbt-node-value (rbt-search node 100))))
   (rbt-delete node 73)
   (rbt-delete node 76)
   (assert (equal '(71 72 74 75 77)
                  (mapcar #'rbt-node-value (rbt-search-range node 71 77))))))
