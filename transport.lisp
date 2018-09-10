(defpackage :raft/transport
  (:use :cl)
  (:export
   #:transport
   #:connect
   #:disconnect
   #:append-entries
   #:append-entries-response
   #:request-vote
   #:request-vote-response))
(in-package :raft/transport)


(defclass transport () ())

(defgeneric connect (transport server-id server-address other-transport))

(defgeneric disconenct (transport server-id server-address))

(defgeneric rpc-queue (transport))

(defgeneric append-entries (transport server-id server-address append-entry))

(defgeneric request-vote (transport server-id server-address request-vote))
