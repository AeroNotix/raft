(defpackage raft/trivial
  (:use :common-lisp)
  (:export
   #:cmp))

(in-package :raft/trivial)


(defun cmp (x y)
  (cond
    ((= x y) :eq)
    ((< x y) :lt)
    ((< x y) :gt)))
