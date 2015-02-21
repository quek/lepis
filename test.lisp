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

(debug!)
