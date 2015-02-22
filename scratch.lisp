(defpackage :lepis.scratch
  (:use :cl :lepis))

(in-package :lepis.scratch)

(defvar *db* (open-db "/tmp/lepis.scratch/"))

(! *db* :hello :world)
(! *db* 'hello 'world)
(! *db* "hello" "world")

(@ *db* :hello)
;;⇒ :WORLD
;;   T
(@ *db* 'hello)
;;⇒ WORLD
;;   T
(@ *db* "hello")
;;⇒ "world"
;;   T
(@ *db* 'no-such-key)
;;⇒ NIL
;;   NIL

(inc *db* :inc)
;;⇒ 1
(inc *db* :inc 10)
;;⇒ 11
(inc *db* :inc -1)
;;⇒ 10
