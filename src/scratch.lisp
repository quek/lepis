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


(handler-case (sb-posix:open "/eetmp/lock" (logior sb-posix:o-wronly sb-posix:o-creat) #8r666)
  (sb-posix:syscall-error (error) (describe error)))
;;→ #<SB-POSIX:SYSCALL-ERROR {10077EBC13}>
;;     [condition]
;;   
;;   Slots with :INSTANCE allocation:
;;     ERRNO  = 2
;;     NAME   = SB-POSIX::OPEN-WITH-MODE
;;   
;;⇒ 


(setq fd (sb-posix:open "/tmp/lepis/lock" (logior sb-posix:o-wronly sb-posix:o-creat) #8r666))
;;⇒ 91

;;⇒ 7
(sb-posix:fcntl fd sb-posix:f-setlk (make-instance 'sb-posix:flock
                                                   :type sb-posix:f-wrlck
                                                   :whence sb-posix:seek-set
                                                   :start 0
                                                   :len 0))
;;⇒ 0

;;⇒ 0


(sb-posix:fcntl fd sb-posix:f-setlk (make-instance 'sb-posix:flock
                                                   :type sb-posix:f-unlck
                                                   :whence sb-posix:seek-set
                                                   :start 0
                                                   :len 0))
;;⇒ 0

(sb-posix:close fd)
;;⇒ 0


(lepis.util::lock-file "/tmp/lock1")
;;⇒ 91
(lepis.util::unlock-file 91)
;;⇒ T

;;→ #<SB-POSIX:SYSCALL-ERROR {100C6EC5E3}>
;;     [condition]
;;   
;;   Slots with :INSTANCE allocation:
;;     ERRNO  = 11
;;     NAME   = SB-POSIX::FCNTL-WITH-POINTER-ARG
;;   
;;⇒ NIL
