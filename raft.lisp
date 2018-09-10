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

(define-state-handler raft :leader (imt state (ae raft/msgs:append-entries))
  ;; check if term > our term
  ;; apply to memory- log, replicate to followers
  state)

(define-state-handler raft :candidate (imt state (ae raft/msgs:append-entries))
  state)

(define-state-handler raft :follower (imt state (rv raft/msgs:request-vote))
  state)

(define-state-handler raft :leader (imt state (rv raft/msgs:request-vote))
  state)

(define-state-handler raft :candidate (imt state (rv raft/msgs:request-vote))
  state)
