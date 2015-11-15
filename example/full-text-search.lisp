(defpackage :lepis.full-text-search
  (:use :cl :lepis))

(in-package :lepis.full-text-search)

(defvar *word-hash-key* '*word-hash-key* "document をキー、ワードリストを値としたハッシュ")

(defun search-key (word)
  "search 用の zset"
  (format nil "s ~a" word))

(defclass document ()
  ((title :initarg :title :accessor title)
   (body  :initarg :body  :accessor body)))

(defmethod print-object ((document document) stream)
  (print-unreadable-object (document stream :type t :identity t)
    (format stream "~a" (title document))))

(defclass search-mixin ()
  ())

(defmethod search-fields ((document document))
  '(title body))

(defmethod update-index (self)
  (flet ((str ()
           (with-output-to-string (out)
             (loop for i in (search-fields self)
                   for word = (cond ((symbolp i)
                                    (slot-value self i))
                                   ((functionp i)
                                    (funcall i self)))
                   if word
                     do (princ word out))))
         (add (word)
           (zinc (search-key word) self)))
    (remove-index self)
    (let ((words (tokenize (str))))
      (loop for s in words
            do (add s))
      (hset *word-hash-key* self (remove-duplicates words :test #'equal)))))

(defmethod remove-index (self)
  (loop for word in (hget *word-hash-key* self)
        do (zrem (search-key word) self)))

(defun tokenize (string)
  (flet ((normalize ()
           (reduce (lambda (s f) (funcall f s))
                   (list #'string-downcase
                         (lambda (s)
                           (ppcre:regex-replace-all "[。、,.\\s]" s "")))
                   :initial-value string)))
    (loop with str = (normalize)
          for i from 0 below (1- (length str))
          for s = (subseq str i (+ i 2))
          collect s)))

(defun search-index (string &key (with-scores nil))
  (let ((words (tokenize string)))
    (when words
      (with-temp-keys (dest)
        (zinterstore dest (mapcar #'search-key words))
        (zrang dest 0 nil :from-end t :with-scores with-scores)))))

(defun clear-index ()
  (mapc #'del (keys "^s ")))

(defun example ()
  (clear-index)
  (update-index (make-instance 'document
                               :title  "オートミール"
                               :body "穀物粉末9 牛乳3 食用蜂蜜2 タマネギ3"))
  (update-index (make-instance 'document
                               :title "柔らかいパン"
                               :body "生地6 発酵剤2 卵2 牛乳3"))
  (update-index (make-instance 'document
                               :title "暗黒プリン"
                               :body "オートミール1 野菜漬け1 鳥肉5 動物の血7"))
  (mapc (lambda (q)
          (format t "~&~a => ~a" q (search-index q)))
        '("オートミール" "牛乳"))
  nil)

(example)
;;→ オートミール => (#<DOCUMENT 暗黒プリン {10100C8E93}> #<DOCUMENT オートミール {100FFE0403}>)
;;   牛乳 => (#<DOCUMENT 柔らかいパン {1010055763}> #<DOCUMENT オートミール {100FFE0403}>)
;;⇒ NIL

