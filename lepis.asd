(asdf:defsystem :lepis
  :version "0.0.0"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "util")
                             (:file "tree")
                             (:file "skip-list")
                             (:file "zset")
                             (:file "set")
                             (:file "package")
                             (:file "dump")
                             (:file "emit-standard-object")
                             (:file "lepis")
                             (:file "interrupt"))))
  :depends-on (anaphora cl-ppcre))
