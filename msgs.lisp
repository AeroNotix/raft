(defpackage raft/msgs
  (:use :cl)
  (:export
   #:append-entries
   #:append-entries-response))

(in-package :raft/msgs)


(defclass append-entries ()
  ((term
    :initarg :term
    :initform nil
    :accessor term
    :documentation "leader's term")
   (leader-id
    :initarg :leader-id
    :accessor leader-id
    :documentation "so follower can redirect clients prev-log-index
index of log entry immediately preceding new ones")
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

(defclass append-entries-response ()
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
