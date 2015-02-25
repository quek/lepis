(asdf:defsystem :lepis
  :version "0.0.0"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "tree")
                             (:file "zset")
                             (:file "package")
                             (:file "lepis"))))
  :depends-on (anaphora))

