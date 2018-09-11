(defpackage :raft/transport
  (:use :cl)
  (:export
   #:transport
   #:connect
   #:disconnect
   #:local-address
   #:rpc-channel
   #:append-entries
   #:append-entries-response
   #:request-vote
   #:request-vote-response))
(in-package :raft/transport)


(defclass transport () ())

(defgeneric connect (transport server-id server-address other-transport))

(defgeneric disconenct (transport server-id server-address))

(defgeneric local-address (transport))

(defgeneric rpc-channel (transport))

(defgeneric append-entries (transport server-id server-address append-entry))

(defgeneric request-vote (transport server-id server-address request-vote))
