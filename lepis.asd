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
                             (:file "lepis"))))
  :depends-on (anaphora))

