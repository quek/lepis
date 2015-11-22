(in-package :lepis.test)

(defstruct foo
  a b)

(defclass bar ()
  ((a :initarg :a :initform nil)
   (b :initarg :b :initform nil)))


(deftest test-basic ()
  (with-db  ("/tmp/lepis/")
    (clear-db)
    (! :foo 1)
    (is (= 1 (@ :foo)))
    (! "hello" "world")
    (is (string= "world" (@ "hello")))
    (! :foo1 (make-foo :a 1 :b 'xxx))
    (is (equalp (make-foo :a 1 :b 'xxx) (@ :foo1))))
  (with-db ("/tmp/lepis/")
    (is (= 1 (@ :foo)))
    (is (string= "world" (@ "hello")))
    (is (equalp (make-foo :a 1 :b 'xxx) (@ :foo1)))))

(deftest test-inc-thread ()
  (with-db ("/tmp/lepis/")
    (clear-db)
    (mapc #'sb-thread:join-thread
          (loop repeat 10
                collect (sb-thread:make-thread
                         (lambda (*db*)
                           (loop repeat 1000
                                 do (inc :inc)))
                         :arguments (list *db*))))
    (is (= 10000 (@ :inc)))))

(deftest test-hash-basic ()
  (with-db ("/tmp/lepis/")
    (clear-db)
    (hset :hash 'a 1 'b 2 'c 3)
    (is (= 2 (hget :hash 'b)))
    (let ((h (hget :hash 'b 'c)))
      (is (= 2 (hash-table-count h)))
      (is (= 2 (gethash 'b h)))
      (is (= 3 (gethash 'c h))))
    (is (= 1 (hdel :hash 'b 'd)))
    (let ((h (hget :hash)))
      (is (= 2 (hash-table-count h)))
      (is (= 1 (gethash 'a h)))
      (is (= 3 (gethash 'c h))))))

(deftest test-zset-basic ()
  (with-db ("/tmp/lepis/")
    (clear-db)
    (is (= 3 (zadd :zset 1 'foo 2 'bar 30 'baz)))
    (is (= 3 (zcard :zset)))
    (is (= 0 (zrank :zset 'foo)))
    (is (= 1 (zrank :zset 'bar)))
    (is (= 2 (zrank :zset 'baz)))
    (is (= 1 (zadd :zset 3 'baz 4 'foz)))
    (is (= 4 (zcard :zset)))
    (is (= 2 (zinc :zset 'foo)))
    (is (= 12 (zinc :zset 'foo 10)))
    (is (= 1 (zinc :zset 'foo -11)))
    (is (equal '(foo bar baz foz) (zrang :zset 0 nil)))
    (is (equal '(foo bar baz foz) (zrang :zset 0 100)))
    (is (equal '(foz baz bar foo) (zrang :zset 0 100 :from-end t)))
    (is (equal '(baz bar) (zrang :zset 1 2 :from-end t)))
    (is (equal '((bar . 2) (baz . 3)) (zrang :zset 1 2 :with-scores t)))
    (is (equal '(bar baz) (zrang-by-score :zset 2 3)))
    (is (equal '((bar . 2) (baz . 3))
               (zrang-by-score :zset most-negative-double-float most-positive-double-float
                               :with-scores t :offset 1 :limit 2)))
    (is (= 2 (zrem :zset 'bar 'foz)))
    (is (equal '(foo baz) (zrang :zset 0 nil))))
  (with-db ("/tmp/lepis/")
    (is (equal '(foo baz) (zrang :zset 0 nil)))
    (is (= 1 (zrem :zset 'baz)))
    (is (equal '(foo) (zrang :zset 0 nil)))
    (is (= 1 (zrem :zset 'foo)))
    (is (equal nil (zrang :zset 0 nil)))))

(deftest test-zset-add-rem-add-rem ()
  (with-db ("/tmp/lepis/")
    (clear-db)
    (zadd "s %%%" 1 'a 2 'b)
    (zrem "s %%%" 'a)
    (zadd "s %%%" 1 'a)
    (zrem "s %%%" 'b)
    (is (equal '(a) (zrang "s %%%" 0 nil)))))

(deftest test-zinterstore ()
  (with-db ("/tmp/lepis/")
    (clear-db)
    (zadd :z1 1 'foo 2 'bar 3 'baz)
    (zadd :z2 20 'bar 30 'baz 40 'foz)
    (zinterstore :z3 '((:z1 2) :z2) :aggregate #'*)
    (is (equal '((bar . 80) (baz . 180)) (zrang :z3 0 nil :with-scores t)))
    (zinterstore :z3 '(:z99999 :z2) :aggregate #'*)
    (is (equal nil (zrang :z3 0 nil :with-scores t)))))

(deftest test-zset-struct ()
  (with-db ("/tmp/lepis/")
    (clear-db)
    (let ((a (make-foo :a 1))
          (b (make-foo :a 1))
          (c (make-foo :a 2)))
      (is (= 2 (zadd :zset 1 a 2 b 3 c)))
      (is (equalp `((,a . 2) (,c . 3)) (zrang :zset 0 nil :with-scores t)))
      (is (= 0 (zadd :zset 20 a)))
      (is (equalp `((,c . 3) (,a . 20)) (zrang :zset 0 nil :with-scores t))))))

(deftest test-zset-class ()
  (with-db ("/tmp/lepis/")
    (clear-db)
    (let ((a (make-instance 'bar :a 1 :b "1"))
          (b (make-instance 'bar :a 2 :b "2")))
      (is (= 2 (zadd :zset 1 a 2 b)))
      (is (equalp `(,a ,b) (zrang :zset 0 nil)))))
  (with-db ("/tmp/lepis/")
    (destructuring-bind (a b) (zrang :zset 0 nil)
      (is (= 1 (slot-value a 'a)))
      (is (string= "1" (slot-value a 'b)))
      (is (= 2 (slot-value b 'a)))
      (is (string= "2" (slot-value b 'b))))))

(deftest test-set-basic ()
  (with-db ("/tmp/lepis/")
    (clear-db)
    (is (= 1 (sadd :set "a")))
    (is (= 2 (sadd :set "a" "b" "c")))
    (is (= 3 (scard :set)))
    (is (= 1 (srem :set "b" "d")))
    (is (= 2 (scard :set)))
    (is (= 2 (sadd :set2 "c" "d")))
    (is (equal '("a") (sdiff :set :set2)))
    (is (equal '("c") (sinter :set :set2)))
    (is (equal '("a" "c" "d") (sunion :set :set2)))))

(deftest test-expire-test ()
  (with-db ("/tmp/lepis/")
    (clear-db)
    (! :a :a)
    (expire :a 1)
    (is (eq (@ :a) :a))
    (sleep 1)
    (is (eq (@ :a) nil))
    (! :a :aa)
    (expire :a 1))
  (with-db ("/tmp/lepis/")
    (is (eq (@ :a) :aa))
    (sleep 1)
    (is (eq (@ :a) nil))))

(deftest test-expire-thread ()
  (with-db ("/tmp/lepis/" :expire-period-second 1)
    (clear-db)
    (! :a :a)
    (expire :a 1)
    (let ((count (hash-table-count (lepis::db-hash *db*))))
      (sleep 1.1)
      (is (= (1- count)
             (hash-table-count (lepis::db-hash *db*)))))))


(deftest test-dump-object-identity ()
  (with-db ("/tmp/lepis/")
    (clear-db)
    (let ((foo (make-foo))
          (list (list 1)))
      (! :list1 list)
      (! :foo1 foo)
      (! :list2 list)
      (! :foo2 foo)
      (zadd :zset 1 list 2 foo)
      (sadd :set foo list)))
  (with-db ("/tmp/lepis/")
    (let ((list (@ :list1))
          (foo (@ :foo1)))
      (@ :list1 list)
      (is (equalp (make-foo) foo))
      (is (eq list (@ :list2)))
      (is (eq foo (@ :foo2)))
      (let ((xs (zrang :zset 0 nil)))
        (is (eq list (car xs)))
        (is (eq foo (cadr xs))))
      (let ((xs (smembers :set)))
        (is (or (eq list (car xs)) (eq list (cadr xs))))
        (is (or (eq foo (car xs)) (eq foo (cadr xs))))))))

(deftest test-open-by-many-threads ()
  (with-db ("/tmp/lepis/")
    (clear-db))
  (mapc #'sb-thread:join-thread
        (loop for i below 10
              collect (sb-thread:make-thread
                       (lambda ()
                         (sleep 0.001)
                         (loop for i below 100 do
                           (with-db ("/tmp/lepis/")
                             (inc :foo)))))))
  (with-db ("/tmp/lepis/")
    (is (= 1000 (@ :foo)))))

(run-package-tests :interactive t)
