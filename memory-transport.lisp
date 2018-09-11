(defpackage :raft/memory-transport
  (:use :cl :raft/transport)
  (:export :memory-transport))
(in-package :raft/memory-transport)


(defclass memory-transport (transport)
  ((peers
    :initarg :peers
    :initform (make-hash-table)
    :accessor peers)
   (rpc-channel
    :initform (make-instance 'chanl:bounded-channel :size 128)
    :accessor rpc-channel)
   (peers-lock
    :accessor peers-lock)))

(defmethod initialize-instance :after ((mt memory-transport) &key)
  (setf (peers-lock mt) (bt:make-lock (format nil "~A" mt))))

(defmethod connect ((mt memory-transport) server-id server-address (omt memory-transport))
  (bt:with-lock-held ((peers-lock mt))
    (setf (gethash server-address (peers mt)) omt)))

(defmethod disconnect ((mt memory-transport) server-id server-address)
  (bt:with-lock-held ((peers-lock mt))
    (remhash server-address (peers mt))))

(defmethod append-entries ((mt memory-transport) server-id server-address (ae raft/msgs:append-entries))
  (let ((peer (bt:with-lock-held ((peers-lock mt))
                (gethash server-address (peers mt))))
        (rpc-reply-channel (make-instance 'chanl:bounded-channel)))
    (chanl:send (rpc-channel peer) ae :blockp nil)
    (chanl:recv rpc-reply-channel)))
