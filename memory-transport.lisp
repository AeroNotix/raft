(defpackage :raft/memory-transport
  (:use :cl
        :raft/state
        :raft/fsm
        :raft/transport))
(in-package :raft/memory-transport)


(defclass memory-transport (transport)
  ((peers
    :initarg :peers
    :initform (make-hash-table)
    :accessor peers)))
