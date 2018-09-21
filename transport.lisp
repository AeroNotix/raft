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

(defgeneric connect (transport server-address)
  (:documentation "Connect to a peer"))

(defgeneric disconnect (transport server-address)
  (:documentation "Disconnect from a peer"))

(defgeneric rpc-channel (transport)
  (:documentation "Return the primitive rpc channel that this
  transport uses"))

(defgeneric append-entries (transport server-address append-entry)
  (:documentation "Relay an AppendEntries RPC to the provided server
  address over this transport"))

(defgeneric request-vote (transport server-address request-vote)
  (:documentation "Relay a RequestVote RPC to the provided server
  address over this transport"))

(defgeneric request-vote-response (transport server-address request-vote-request)
  (:documentation "Relay a response to a RequestVote RPC to the
  provided server address over this transport"))

(defgeneric hangup (transport)
  (:documentation "Close this transport"))
