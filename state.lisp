(defpackage raft/state
  (:use :common-lisp)
  (:export #:raft-state
           #:current-term
           #:voted-for
           #:log-entries
           #:commit-index
           #:last-applied
           #:match-index
           #:current-state
           #:heartbeat-timout
           #:last-log-index
           #:last-log-term))

(in-package :raft/state)


(defclass raft-state ()
  (;; Slots and documentation strings are taken directly
   ;; from the raft paper: https://raft.github.io/raft.pdf
   ;; persistent state on all servers:
   (current-term
    :initform 0
    :accessor current-term
    :documentation "latest term server has seen (initialized to 0 on
first boot, increases monotonically")
   (voted-for
    :initform nil
    :accessor voted-for
    :documentation "candidate-id that received vote in current
term (or null if none)")
   (log-entries
    :initform nil
    :accessor log-entries
    :documentation "log entries; each entry contains command for state
machine, and term when entry was received by leader (first index is
1)")
   ;; volatile state on all servers:
   (commit-index
    :initform 0
    :accessor commit-index
    :documentation "index of highest log entry known to be
committed (initialized to 0, increases monotonically)")
   (last-applied
    :initform 0
    :accessor last-applied
    :documentation "index of highest log entry applied to state
machine (initialized to 0, increases monotonically)")

   ;; volatile state on leaders (reinitialize after alection)
   (next-index
    :initform (make-hash-table)
    :accessor next-index
    :documentation "for each server, index of the next log entry to
send to that server (initialized to leader last log index + 1)")
   (match-index
    :initform (make-hash-table)
    :accessor match-index
    :documentation "for each server, index of highest log entry known
to be replicated on server (initialized to 0, increases
monotonically)")

   ;; Implementation-specific slots
   (current-state
    :initform :follower
    :accessor current-state
    :documentation "Each server should start in the follower state,
transitioning between raft states as described in the paper. Possible
modes: :follower, :candidate, :leader")))


(defmethod last-log-index ((rs raft-state))
  (warn "last-log-index not doing the right thing")
  0)

(defmethod last-log-term ((rs raft-state))
  (warn "last-log-term not doing the right thing")
  0)
