(defpackage :raft/discovery
  (:use :cl)
  (:export
   #:discoverer
   #:find-peers
   #:background-find-peers))
(in-package :raft/discovery)

#|

Typically the discovery implementation will be tied to the transport
implementation. I.e. the memory discovery mechanism relies on the
transport's implementation. Doesn't have to be like this, however.

|#

(defclass discoverer () ())

(defgeneric find-peers (discoverer)
  (:documentation "Use the discoverer to find possible peers"))

(defgeneric background-find-peers (discoverer)
  (:documentation "Continually use the discoverer to find possible peers"))
