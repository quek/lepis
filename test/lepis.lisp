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

(def-test inc-thread ()
  (with-db (db "/tmp/lepis/")
    (clear-db db)
    (mapc #'sb-thread:join-thread
          (loop repeat 10
                collect (sb-thread:make-thread
                         (lambda ()
                           (loop repeat 1000
                                 do (inc db :inc))))))
    (is (= 10000 (@ db :inc)))))

(def-test zset-basic ()
  (with-db (db "/tmp/lepis/")
    (clear-db db)
    (is (= 3 (zadd db :zset 1 'foo 2 'bar 30 'baz)))
    (is (= 3 (zcard db :zset)))
    (is (= 0 (zrank db :zset 'foo)))
    (is (= 1 (zrank db :zset 'bar)))
    (is (= 2 (zrank db :zset 'baz)))
    (is (= 1 (zadd db :zset 3 'baz 4 'foz)))
    (is (= 4 (zcard db :zset)))
    (is (equal '(foo bar baz foz) (zrang db :zset 0 nil)))
    (is (equal '((bar . 2) (baz . 3)) (zrang db :zset 1 2 :with-scores t)))
    (is (equal '(bar baz) (zrang-by-score db :zset 2 3)))
    (is (equal '((bar . 2) (baz . 3))
               (zrang-by-score db :zset most-negative-double-float most-positive-double-float
                               :with-scores t :offset 1 :limit 2)))
    (is (= 2 (zrem db :zset 'bar 'foz)))
    (is (equal '(foo baz) (zrang db :zset 0 nil)))))

(def-test zset-struct ()
  (with-db (db "/tmp/lepis/")
    (clear-db db)
    (let ((a (make-foo :a 1))
          (b (make-foo :a 1))
          (c (make-foo :a 2)))
      (is (= 2 (zadd db :zset 1 a 2 b 3 c)))
      (is (equalp `((,a . 2) (,c . 3)) (zrang db :zset 0 nil :with-scores t)))
      (is (= 0 (zadd db :zset 20 a)))
      (is (equalp `((,c . 3) (,a . 20)) (zrang db :zset 0 nil :with-scores t))))))


(debug!)
