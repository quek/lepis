(defpackage :lepis.util
  (:use :cl :anaphora)
  (:export #:value<
           #:value=
           #:lock-file
           #:unlock-file
           #:tempfile))

(in-package :lepis.util)

(declaim (inline value<))
(defgeneric value< (x y)
  (:method ((x string) (y string))
    (string< x y))
  (:method ((x number) (y number))
    (< x y))
  (:method ((x string) (y number))
    t)
  (:method ((x number) (y string))
    nil)
  (:method ((x string) y)
    t)
  (:method (x (y string))
    nil)
  (:method ((x number) y)
    t)
  (:method (x (y number))
    nil)
  (:method (x y)
    (string< (prin1-to-string x) (prin1-to-string y))))

(declaim (inline value=))
(defgeneric value= (x y)
  (:method ((x string) (y string))
    (string= x y))
  (:method ((x number) (y number))
    (= x y))
  (:method ((x string) y)
    nil)
  (:method (x (y string))
    nil)
  (:method ((x number) y)
    nil)
  (:method (x (y number))
    nil)
  (:method (x y)
    (string= (prin1-to-string x) (prin1-to-string y))))

(defun lock-file (file)
  (handler-case
      (let ((fd (sb-posix:open file (logior sb-posix:o-wronly sb-posix:o-creat) #8r666)))
        (sb-posix:fcntl fd sb-posix:f-setlk (make-instance 'sb-posix:flock
                                                           :type sb-posix:f-wrlck
                                                           :whence sb-posix:seek-set
                                                           :start 0
                                                           :len 0))
        fd)
    (sb-posix:syscall-error (error)
      (describe error)
      nil)))

(defun unlock-file (fd)
  (handler-case
      (progn
        (sb-posix:fcntl fd sb-posix:f-setlk (make-instance 'sb-posix:flock
                                                           :type sb-posix:f-unlck
                                                           :whence sb-posix:seek-set
                                                           :start 0
                                                           :len 0))
        (sb-posix:close fd)
        t)
    (sb-posix:syscall-error (error)
      (describe error)
      nil)))

(defun tempfile (dir)
  (multiple-value-bind (fd file)
      (sb-posix:mkstemp
       (namestring
        (merge-pathnames "dump-temp-XXXXXX" dir)))
    (sb-posix:close fd)
    file))
