(defpackage :lepis.test
  (:use :cl :lepis :fiveam)
  (:shadowing-import-from :lepis #:!))

(in-package :lepis.test)

(def-suite all)
