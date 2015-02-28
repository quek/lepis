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
    (is (= 3 (lepis.tree::node-size node)))

    (setf node (lepis.tree:tree-add node 4 4))
    (setf node (lepis.tree:tree-add node 2 20))
    (setf node (lepis.tree:tree-add node 2 0))
    (is (equal '(1 0 2 20 3 4) (mapcar #'lepis.tree::node-value
                                       (lepis.tree::tree-search-range-by-rank node 0))))

    (setf node (lepis.tree:tree-add node 3 30))
    (is (equal '(0 2 20 3 30)
               (mapcar #'lepis.tree::node-value
                       (lepis.tree:tree-search-range-by-score node 2 3))))

    (setf node (lepis.tree:tree-delete node 2 20))
    (setf node (lepis.tree:tree-delete node 3 30))
    (is (equal '(1 0 2 3 4) (mapcar #'lepis.tree::node-value
                                    (lepis.tree::tree-search-range-by-rank node 0))))))

(debug!)
