(defpackage :raft/discovery
  (:use :cl)
  (:export
   #:discoverer
   #:find-peers
   #:background-find-peers))
(in-package :raft/discovery)


(defclass discoverer () ())

(defgeneric find-peers (discoverer)
  (:documentation "Use the discoverer to find possible peers"))

(defgeneric background-find-peers (discoverer)
  (:documentation "Continually use the discoverer to find possible peers"))
