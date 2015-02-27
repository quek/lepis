(in-package :lepis.test)

(def-suite tree :in all)
(in-suite tree)

(def-test basic ()
  (let ((node nil))
    (setf node (lepis.tree:tree-add node 1 1))
    (is (= 1 (lepis.tree::node-size node)))
    (setf node (lepis.tree:tree-add node 2 2))
    (is (= 2 (lepis.tree::node-size node)))
    (is (= 1 (lepis.tree::node-key node)))
    (is (= 2 (lepis.tree::node-key (lepis.tree::node-right node))))
    (setf node (lepis.tree:tree-add node 3 3))
    (is (= 3 (lepis.tree::node-size node)))))

(debug!)
