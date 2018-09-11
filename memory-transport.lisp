(defpackage :raft/memory-transport
  (:use :cl :raft/transport)
  (:export :memory-transport))
(in-package :raft/memory-transport)


(defclass memory-transport (transport)
  ((serializer
    :initarg serializer
    :initform (raft/serialization:make-basic-serializer)
    :accessor serializer)
   (peers
    :initarg :peers
    :initform (make-hash-table)
    :accessor peers)
   (rpc-channel
    :initform (make-instance 'chanl:bounded-channel :size 128)
    :accessor rpc-channel)
   (peers-lock
    :accessor peers-lock)))


(defmethod trivial-rpc ((mt memory-transport) server-address (rpc raft/msgs:raft-request))
  (log:debug "~A sending ~A to ~A" mt server-address rpc)
  (let ((peer (bt:with-lock-held ((peers-lock mt))
                (gethash server-address (peers mt)))))
    (if peer
        (chanl:send (rpc-channel peer) rpc :blockp nil)
        (log:warn "~A attempted to ~A on a peer we have not connected to" mt rpc))))

(defmethod initialize-instance :after ((mt memory-transport) &key)
  (setf (peers-lock mt) (bt:make-lock (format nil "~A" mt))))

(defmethod connect ((mt memory-transport) server-address (omt memory-transport))
  (bt:with-lock-held ((peers-lock mt))
    (setf (gethash server-address (peers mt)) omt)))

(defmethod disconnect ((mt memory-transport) server-address)
  (bt:with-lock-held ((peers-lock mt))
    (remhash server-address (peers mt))))

(defmethod append-entries ((mt memory-transport) server-address (ae raft/msgs:append-entries))
  (trivial-rpc mt server-address ae))

(defmethod encode-peer ((mt memory-transport) server-address)
  server-address)

(defmethod request-vote ((mt memory-transport) server-address (rv raft/msgs:request-vote))
  (trivial-rpc mt server-address rv))
