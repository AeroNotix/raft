(defpackage raft
  (:use :common-lisp)
  (:import-from #:raft/disk
                #:persistent-hash-table
                #:log-entry
                #:operation))


(defclass raft ()
  ((transport
    :initarg :transport
    :initform nil
    :accessor transport)
   (peers
    :initarg :peers
    :initform (make-hash-table)
    :accessor peers)
   (raft-state
    :initform (make-instance 'raft-state)
    :accessor raft-state)))

(defmethod raft/fsm:state ((imt raft))
  (current-state (raft-state imt)))

(defmethod (setf raft/fsm:state) (state (imt raft))
  (setf (current-state (raft-state imt)) state))

(define-state-handler raft :follower (imt state (ae raft/msgs:append-entries))
  state)

(define-state-handler raft :leader (imt state (ae raft/msgs:append-entries))
  state)

(define-state-handler raft :candidate (imt state (ae raft/msgs:append-entries))
  state)

(define-state-handler raft :follower (imt state (rv raft/msgs:request-vote))
  state)

(define-state-handler raft :leader (imt state (rv raft/msgs:request-vote))
  state)

(define-state-handler raft :candidate (imt state (rv raft/msgs:request-vote))
  state)
