(defpackage :raft/transport
  (:use :cl)
  (:export
   #:transport
   #:server-id
   #:serializer
   #:connect
   #:disconnect
   #:rpc-channel
   #:append-entries
   #:append-entries-response
   #:request-vote
   #:request-vote-response
   #:encode-peer
   #:hangup))
(in-package :raft/transport)


(defclass transport ()
  ((server-id
    :initarg :server-id
    :initform (error "Transports require a server-id, else they cannot uniquely identify themselves")
    :accessor server-id)
   (serializer
    :initarg :serializer
    :initform (raft/serialization:make-basic-serializer)
    :accessor serializer)))

(defgeneric connect (transport server-address))

(defgeneric disconnect (transport server-address))

(defgeneric rpc-channel (transport))

(defgeneric append-entries (transport server-address append-entry))

(defgeneric request-vote (transport server-address request-vote))

(defgeneric request-vote-response (transport server-address request-vote-request))

(defgeneric encode-peer (transport server-address))

(defgeneric hangup (transport))
