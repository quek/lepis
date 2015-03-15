(in-package :lepis.test)

(defsuite lepis.tree)

(in-suite lepis.tree)

(deftest basic ()
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
                                       (lepis.tree::tree-search-range-by-rank node 0 nil nil))))

    (setf node (lepis.tree:tree-add node 3 30))
    (is (equal '(0 2 20 3 30)
               (mapcar #'lepis.tree::node-value
                       (lepis.tree:tree-search-range-by-score node 2 3 0 nil))))
    (is (equal '(2 20)
               (mapcar #'lepis.tree::node-value
                       (lepis.tree:tree-search-range-by-score node 2 3 1 2))))

    (setf node (lepis.tree:tree-delete node 2 20))
    (setf node (lepis.tree:tree-delete node 3 30))
    (is (equal '(1 0 2 3 4) (mapcar #'lepis.tree::node-value
                                    (lepis.tree::tree-search-range-by-rank node 0 nil nil))))))

(lepis.tree)
