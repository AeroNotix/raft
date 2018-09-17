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
   #:request-vote-response
   #:encode-peer))
(in-package :raft/transport)


(defclass transport ()
  ((serializer
    :initarg serializer
    :initform (raft/serialization:make-basic-serializer)
    :accessor serializer)))

(defgeneric connect (transport server-address other-transport))

(defgeneric disconnect (transport server-address))

(defgeneric local-address (transport))

(defgeneric rpc-channel (transport))

(defgeneric append-entries (transport server-address append-entry))

(defgeneric request-vote (transport server-address request-vote))

(defgeneric request-vote-response (transport request-vote-request term successful-p))

(defgeneric encode-peer (transport server-address))
