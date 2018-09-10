(defpackage :raft/transport
  (:use :cl)
  (:export
   #:append-entries
   #:append-entries-response
   #:request-vote
   #:request-vote-response))
(in-package :raft/transport)


(defgeneric append-entries (transport server-id server-address append-entry))

(defgeneric append-entries-response (transport server-id server-address append-entry-response))

(defgeneric request-vote (transport server-id server-address request-vote))

(defgeneric request-vote-response (transport server-id server-address request-vote-response))


(defclass transport () ())
