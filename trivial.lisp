(defpackage raft/trivial
  (:use :common-lisp)
  (:export
   #:cmp
   #:while))

(in-package :raft/trivial)


(defun cmp (x y)
  (cond
    ((= x y) :eq)
    ((< x y) :lt)
    ((< x y) :gt)))

(defmacro while (predicate &body body)
  `(loop
      while ,predicate
      do
        (progn ,@body)))
