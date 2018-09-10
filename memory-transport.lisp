(defpackage :raft/memory-transport
  (:use :cl
        :raft/state
        :raft/fsm
        :raft/transport))
(in-package :raft/memory-transport)

(define-state-machine memory-transport :follower ()
  ((peers
    :initarg :peers
    :initform (make-hash-table)
    :accessor peers)
   (raft-state
    :initform (make-instance 'raft-state)
    :accessor raft-state)))

(defun make-memory-transport ())

(defmethod raft/fsm:state ((imt memory-transport))
  (current-state (raft-state imt)))

(defmethod (setf raft/fsm:state) (state (imt memory-transport))
  (setf (current-state (raft-state imt)) state))

(define-state-handler memory-transport :follower (imt state (ae raft/msgs:append-entries))
  ;;   Receiver implementation:
  ;; 1. Reply false if term < currentTerm (§5.1)
  ;; 2. Reply false if log doesn’t contain an entry at prevLogIndex
  ;; whose term matches prevLogTerm (§5.3)
  ;; 3. If an existing entry conflicts with a new one (same index
  ;; but different terms), delete the existing entry and all that
  ;; follow it (§5.3)
  ;; 4. Append any new entries not already in the log
  ;; 5. If leaderCommit > commitIndex, set commitIndex =
  ;; min(leaderCommit, index of last new entry)
  state)

(define-state-handler memory-transport :leader (imt state (ae raft/msgs:append-entries))
  ;; check if term > our term
  ;; apply to memory- log, replicate to followers
  state)

(define-state-handler memory-transport :candidate (imt state (ae raft/msgs:append-entries))
  state)

(define-state-handler memory-transport :follower (imt state (rv raft/msgs:request-votes))
  state)

(define-state-handler memory-transport :leader (imt state (rv raft/msgs:request-votes))
  state)

(define-state-handler memory-transport :candidate (imt state (rv raft/msgs:request-votes))
  state)
