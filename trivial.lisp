(defpackage raft/trivial
  (:use :common-lisp)
  (:export
   #:cmp
   #:compose
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

(defun compose (&rest functions)
  (lambda (value)
    (reduce #'funcall functions
            :initial-value value
            :from-end t)))
