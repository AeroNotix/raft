(defpackage :raft/peer
  (:use :cl)
  (:export
   #:peer))
(in-package :raft/peer)


(defclass peer ()
  ((server-id
    :initarg :server-id
    :accessor server-id)
   (server-address
    :initarg :server-address
    :accessor server-address)))
