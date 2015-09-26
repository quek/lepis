(defpackage :lepis
 (:use :cl :anaphora :lepis.util :lepis.zset :lepis.set)
 (:export #:*db*
          #:open-db
          #:close-db
          #:with-db
          #:load-db
          #:dump-db
          #:clear-db
          #:keys
          #:!
          #:@
          #:del
          #:expire
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
          #:smembers
          #:zcard
          #:zadd
          #:zrang
          #:zrang-by-score
          #:zrank
          #:zrem))
