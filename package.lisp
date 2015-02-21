(defpackage :lepis
 (:use :cl :anaphora)
 (:export #:open-db
          #:close-db
          #:with-db
          #:load-db
          #:dump-db
          #:clear-db
          #:!
          #:@))
