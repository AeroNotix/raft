(defpackage :raft/conditions
  (:use :cl)
  (:export :log-entry-too-old))

(in-package :raft/conditions)


(define-condition log-entry-too-old (error)
  ((attempted-index
    :initarg :attempted-index
    :accessor attempted-index)
   (highest-index
    :initarg :highest-index
    :accessor highest-index))
   (:report
    (lambda (condition stream)
      (format stream "Attempted to apply an old log entry. Attempted index: ~D, highest index: ~D"
              (attempted-index condition)
              (highest-index)))))
