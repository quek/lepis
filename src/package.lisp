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
          #:zadd
          #:zrang
          #:zrang-by-score))
