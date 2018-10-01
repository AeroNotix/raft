(defpackage :raft/memory-transport
  (:use :cl
        :raft/transport
        :raft/discovery)
  (:export #:memory-transport
           #:memory-discoverer
           #:peers
           #:find-peers))
(in-package :raft/memory-transport)

(defparameter *memory-transport-directory-lock* (bt:make-lock "memory-transport-directory-lock"))
(defparameter *memory-transport-directory* (make-hash-table :test 'equal))


(defclass memory-discoverer (discoverer) ())

(defmethod find-peers ((md memory-discoverer))
  (bt:with-lock-held (*memory-transport-directory-lock*)
    (loop for server-id being the hash-keys in *memory-transport-directory*
       collect
         (make-instance 'raft/peer:peer :server-id server-id))))

(defclass memory-transport (transport)
  ((peers
    :initarg :peers
    :initform (make-hash-table)
    :accessor peers)
   (rpc-channel
    :initform (make-instance 'chanl:bounded-channel :size 128)
    :accessor rpc-channel)))

(defmethod trivial-rpc ((mt memory-transport) (p raft/peer:peer) (rpc raft/msgs:raft-request))
  (log:debug "~A sending ~A to ~A" mt rpc p)
  (let ((peer (find-memory-transport mt p)))
    (if peer
        (progn
          (chanl:send (rpc-channel peer) rpc :blockp nil)
          (log:debug "Sent ~A to ~A" rpc peer))
        (log:warn "~A attempted to ~A on a peer we have not connected to" mt rpc))))

(defmethod initialize-instance :after ((mt memory-transport) &key)
  (bt:with-lock-held (*memory-transport-directory-lock*)
    (setf (gethash (server-id mt) *memory-transport-directory*) mt)))

(defmethod find-memory-transport ((mt memory-transport) (p raft/peer:peer))
  (bt:with-lock-held (*memory-transport-directory-lock*)
    (gethash (decode-peer mt p) *memory-transport-directory*)))

(defmethod connect ((mt memory-transport) (p raft/peer:peer))
  (alexandria:when-let ((peer (find-memory-transport mt p)))
    (setf (gethash (decode-peer mt p) (peers mt)) peer)))

(defmethod disconnect ((mt memory-transport) server-address)
  (remhash server-address (peers mt)))

(defmethod append-entries ((mt memory-transport) (p raft/peer:peer) (ae raft/msgs:append-entries))
  (trivial-rpc mt p ae))

(defmethod request-vote ((mt memory-transport) (p raft/peer:peer) (rv raft/msgs:request-vote))
  (trivial-rpc mt p rv))

(defmethod request-vote-response ((mt memory-transport) (p raft/peer:peer) (rv raft/msgs:request-vote-response))
  (trivial-rpc mt p rv))

(defmethod hangup ((mt memory-transport))
  (bt:with-lock-held (*memory-transport-directory-lock*)
    (remhash (server-id mt) *memory-transport-directory*)))

(defmethod encode-peer ((mt memory-transport) server-id server-address)
  (make-instance 'raft/peer:peer :server-id server-id))

(defmethod decode-peer ((mt memory-transport) (p raft/peer:peer))
  (raft/peer:server-id p))
