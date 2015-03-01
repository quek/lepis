(defpackage :lepis
 (:use :cl :anaphora :lepis.zset)
 (:export #:open-db
          #:close-db
          #:with-db
          #:load-db
          #:dump-db
          #:clear-db
          #:!
          #:@
          #:inc
          #:zcard
          #:zadd
          #:zrang
          #:zrang-by-score
          #:zrank))
