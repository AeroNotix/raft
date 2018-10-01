(defpackage :raft/peer
  (:use :cl)
  (:export
   #:peer
   #:server-id
   #:server-address))
(in-package :raft/peer)


(defclass peer ()
  ((server-id
    :initarg :server-id
    :accessor server-id)
   (server-address
    :initarg :server-address
    :accessor server-address)))
