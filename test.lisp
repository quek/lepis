(defpackage :lepis.test
  (:use :cl :lepis :fiveam)
  (:shadowing-import-from :lepis #:! ))

(in-package :lepis.test)

(defstruct foo
  a b)

(def-test basic ()
  (with-db  (db "/tmp/lepis/")
    (clear-db db)
    (! db :foo 1)
    (is (= 1 (@ db :foo)))
    (! db "hello" "world")
    (is (string= "world" (@ db "hello")))
    (! db :foo1 (make-foo :a 1 :b 'xxx))
    (is (equalp (make-foo :a 1 :b 'xxx) (@ db :foo1))))
  (with-db (db "/tmp/lepis/")
    (is (= 1 (@ db :foo)))
    (is (string= "world" (@ db "hello")))
    (is (equalp (make-foo :a 1 :b 'xxx) (@ db :foo1)))))

(def-test inc ()
  (with-db (db "/tmp/lepis/")
    (clear-db db)
    (mapc #'sb-thread:join-thread
          (loop repeat 10
                collect (sb-thread:make-thread
                         (lambda ()
                           (loop repeat 1000
                                 do (inc db :inc))))))
    (is (= 10000 (@ db :inc)))))

(debug!)
