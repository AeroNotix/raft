(defpackage :raft/memory-transport
  (:use :cl :raft/transport)
  (:export #:memory-transport
           #:peers))
(in-package :raft/memory-transport)

(defparameter *memory-transport-directory-lock* (bt:make-lock "memory-transport-directory-lock"))
(defparameter *memory-transport-directory* (make-hash-table :test 'equal))

(defclass memory-transport (transport)
  ((peers
    :initarg :peers
    :initform (make-hash-table)
    :accessor peers)
   (rpc-channel
    :initform (make-instance 'chanl:bounded-channel :size 128)
    :accessor rpc-channel)))


(defmethod trivial-rpc ((mt memory-transport) server-address (rpc raft/msgs:raft-request))
  (log:debug "~A sending ~A to ~A" mt rpc server-address)
  (let ((peer (gethash server-address (peers mt))))
    (if peer
        (chanl:send (rpc-channel peer) rpc :blockp nil)
        (log:warn "~A attempted to ~A on a peer we have not connected to" mt rpc))))

(defmethod initialize-instance :after ((mt memory-transport) &key)
  (bt:with-lock-held (*memory-transport-directory-lock*)
    (setf (gethash (server-id mt) *memory-transport-directory*) mt)))

(defun find-memory-transport (server-address)
  (bt:with-lock-held (*memory-transport-directory-lock*)
    (gethash server-address *memory-transport-directory*)))

(defmethod connect ((mt memory-transport) server-address)
  (alexandria:when-let ((peer (find-memory-transport server-address)))
    (setf (gethash server-address (peers mt)) peer)))

(defmethod disconnect ((mt memory-transport) server-address)
  (remhash server-address (peers mt)))

(defmethod append-entries ((mt memory-transport) server-address (ae raft/msgs:append-entries))
  (trivial-rpc mt server-address ae))

(defmethod encode-peer ((mt memory-transport) server-address)
  server-address)

(defmethod request-vote ((mt memory-transport) server-address (rv raft/msgs:request-vote))
  (trivial-rpc mt server-address rv))

(defmethod request-vote-response ((mt memory-transport) server-address (rv raft/msgs:request-vote-response))
  (trivial-rpc mt server-address rv))

