(asdf:defsystem :lepis.test
  :version "0.0.0"
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "tree")
                             (:file "lepis"))))
  :depends-on (lepis fiasco))

