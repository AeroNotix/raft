(defpackage raft/msgs
  (:use :cl)
  (:export
   #:raft-request
   #:append-entries
   #:prev-log-term
   #:prev-log-index
   #:append-entries-response
   #:success
   #:request-vote
   #:candidate-id
   #:request-vote-response
   #:term
   #:vote-granted)
  (:documentation "All RPC definitions are taken directly from the
Raft paper located at: https://raft.github.io/raft.pdf. In particular
the definitions from page 4."))

(in-package :raft/msgs)


(defclass raft-request () ())

(defclass append-entries (raft-request)
  ((term
    :initarg :term
    :initform nil
    :accessor term
    :documentation "leader's term")
   (leader-id
    :initarg :leader-id
    :accessor leader-id
    :documentation "so follower can redirect clients")
   (prev-log-index
    :initarg :prev-log-index
    :accessor prev-log-index
    :documentation "index of log entry immediately preceding new ones")
   (prev-tog-term
    :initarg :prev-log-term
    :accessor prev-log-term
    :documentation "term of prev-log-index entry")
   (entries
    :initarg :entries
    :initform nil
    :accessor entries
    :documentation "log entries to store (empty for heartbeat; may
send more than one for efficiency)")
   (leader-commit
    :initarg :leader-commit
    :accessor leader-commit
    :documentation "leader's commitIndex")))

(defclass append-entries-response (raft-request)
  ((term
    :initarg :term
    :accessor term
    :documentation "currentTerm, for leader to update itself")
   (success
    :initarg :success
    :initform nil
    :accessor success
    :documentation "true if follower contained entry matching
prev-log-index and prev-log-term")))


(defclass request-vote (raft-request)
  ((term
    :initarg :term
    :accessor term
    :documentation "candidate's term")
   (candidate-id
    :initarg :candidate-id
    :accessor candidate-id
    :documentation "candidate requesting vote")
   (last-log-index
    :initarg :last-log-index
    :accessor last-log-index
    :documentation "index of candidate's last log entry (§5.4)")
   (last-log-term
    :initarg :last-log-term
    :accessor last-log-term
    :documentation "term of candidate's last log entry (§5.4)")))

(defclass request-vote-response (raft-request)
  ((term
    :initarg :term
    :initform 0
    :accessor term
    :documentation "current-term, for candidate to update itself")
   (vote-granted
    :initarg :vote-granted
    :initform nil
    :accessor vote-granted
    :documentation "T means candidate received vote, NIL otherwise")))

(defmethod print-object ((rvr request-vote-response) stream)
  (format stream "#<RequestVoteResponse>{term: ~D; vote-granted: ~A}"
          (term rvr) (vote-granted rvr)))
