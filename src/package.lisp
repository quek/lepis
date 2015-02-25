(defpackage :lepis
 (:use :cl :anaphora :lepis.tree)
 (:export #:open-db
          #:close-db
          #:with-db
          #:load-db
          #:dump-db
          #:clear-db
          #:!
          #:@
          #:inc))
