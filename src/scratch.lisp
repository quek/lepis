(defpackage :lepis.scratch
  (:use :cl :lepis :anaphora))

(in-package :lepis.scratch)

(defvar *db* (open-db "/tmp/lepis.scratch/" :dump-threshold-second 5))
;;(close-db *db*)

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

(zadd *db* :zset 1 :a 2 :b)
;;⇒ 2
(zrang *db* :zset 0 nil :with-scores t)
;;⇒ ((:A . 1) (:B . 2))
(zrang *db* :zset 0 nil)
;;⇒ (:A :B)

(defglobal *x* 'hello)
;;⇒ *X*

(sb-thread:make-thread (lambda ()
                         (print *x*)))
;;⇒ #<SB-THREAD:THREAD FINISHED values: HELLO {100D4BA9E3}>





