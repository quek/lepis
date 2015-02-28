(in-package :lepis.test)

(def-suite lepis :in all)

(in-suite lepis)

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

(def-test zset ()
  (with-db (db "/tmp/lepis/")
    (clear-db db)
    (is (= 3 (zadd db :zset 1 'foo 2 'bar 30 'baz)))
    (is (= 3 (zcard db :zset)))
    (is (= 1 (zadd db :zset 3 'baz  4 'foz)))
    (is (= 4 (zcard db :zset)))
    (is (equal '(foo bar baz foz) (zrang db :zset 0 nil)))
    (is (equal '((foo . 1) (bar . 2) (baz . 3) (foz . 4))
               (zrang db :zset 0 nil :with-scores t)))
    (is (equal '(bar baz) (zrang-by-score db :zset 2 3)))
    (is (equal '((bar . 2) (baz . 3))
               (zrang-by-score db :zset most-negative-double-float most-positive-double-float
                               :with-scores t :offset 1 :limit 2)))))

(debug!)
