(defpackage :lepis.set
  (:use :cl :anaphora :lepis.skip-list)
  (:export #:make-set
           #:set-add
           #:set-remove
           #:set-member-p
           #:set-diff
           #:set-union
           #:set-inter
           #:map-set))

(in-package :lepis.set)

(defun make-set ()
  (make-skip-list))

(defun set-add (set x)
  (skip-list-add set x))

(defun set-remove (set x)
  (skip-list-remove set x))

(defun set-member-p (set x)
  (skip-list-search set x))

(defun set-union (&rest sets)
  (let ((set (make-set)))
    (loop for x in sets
          do (map-skip-list (lambda (value) (set-add set value))
                            x))
    set))

(defun set-diff (set &rest sets)
  (let ((set (set-union set)))          ;TODO 非効率
    (loop for x in sets
          do (map-skip-list (lambda (value) (set-remove set value))
                            (apply #'set-union sets)))
    set))

(defun set-inter (set &rest sets)
  (let ((result-set (make-set)))
    (map-skip-list (lambda (value)
                     (when (every (lambda (set) (skip-list-search set value)) sets)
                       (set-add result-set value)))
                   set)
    result-set))

(defun map-set (function set)
  (map-skip-list function set))
