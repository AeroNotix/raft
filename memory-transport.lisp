(in-package :raft)


(defclass in-memory-transport ()
  ((peers
    :initarg :peers
    :initform (make-hash-table)
    :accessor peers)
   (raft-state
    :initform (make-instance 'raft-state)
    :accessor raft-state)))


(defmethod update-entries ((imt in-memory-transport) (ae raft/msgs::append-entries))
  )

(defmethod append-entries ((imt in-memory-transport) server-id server-address (ae raft/msgs::append-entries))
  (make-instance 'append-entries-response
                 :term (> (term (raft-state imt) (term ae)))
                 :success (update-entries imt ae)))
