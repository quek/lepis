(defpackage :lepis
 (:use :cl :anaphora :lepis.zset :lepis.set)
 (:export #:open-db
          #:close-db
          #:with-db
          #:load-db
          #:dump-db
          #:clear-db
          #:!
          #:@
          #:del
          #:inc
          #:hdel
          #:hget
          #:hset
          #:sadd
          #:scard
          #:srem
          #:sinter
          #:sunion
          #:sdiff
          #:zcard
          #:zadd
          #:zrang
          #:zrang-by-score
          #:zrank
          #:zrem))
