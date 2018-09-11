(defpackage raft
  (:use :common-lisp :chanl)
  (:import-from #:raft/disk
                #:persistent-hash-table
                #:log-entry
                #:operation)
  (:import-from #:raft/transport
                #:rpc-channel)
  (:import-from #:raft/memory-transport
                #:memory-transport)
  (:import-from #:raft/transport
                #:rpc-channel)
  (:import-from #:raft/fsm
                #:define-state-machine
                #:define-state-handler
                #:apply-event
                #:state))
(in-package :raft)


(define-state-machine raft :follower ()
  ((transport
    :initarg :transport
    :initform nil
    :accessor transport)
   (leader
    :accessor leader)
   (votes
    :initform 0
    :accessor votes)
   (heartbeat-timer
    :initform nil
    :accessor heartbeat-timer)
   (heartbeat-channel
    :initform nil
    :accessor heartbeat-channel
    :documentation "A value that will determine if this raft server
has experienced a timeout from not receiving AppendEntries RPCs in a
timely manner")
   (raft-state
    :initform (make-instance 'raft-state)
    :accessor raft-state)))

(defmethod raft/fsm:state ((raft raft))
  (current-state (raft-state raft)))

(defmethod (setf raft/fsm:state) (state (raft raft))
  (setf (current-state (raft-state raft)) state))

(defmethod reset-heartbeat-timer ((raft raft))
  (when (and (heartbeat-timer raft)
             (heartbeat-channel raft))
    ;; todo: wrap the timer type, present it as an opaque object
    (chanl:recv (heartbeat-channel raft) :blockp nil)
    (trivial-timers:unschedule-timer (heartbeat-timer raft))))

(defmethod new-heartbeat-timer ((raft raft))
  (reset-heartbeat-timer raft)
  (multiple-value-bind (hb-channel hb-timer) (raft/timers:after 3)
    (setf (heartbeat-timer raft) hb-timer)
    (setf (heartbeat-channel raft) hb-channel)))

(defmethod start-leader-election ((raft raft))
  (log:warn "Starting leader election due to missed heartbeat: ~A" raft)
  (incf (current-term (raft-state raft)))
  (setf (votes raft) 0))

(define-state-handler raft :follower (r state :heartbeat-timeout)
  (start-leader-election r)
  :candidate)

(define-state-handler raft :follower (r state (ae raft/msgs:append-entries))
  state)

(define-state-handler raft :leader (r state (ae raft/msgs:append-entries))
  state)

(define-state-handler raft :candidate (r state (ae raft/msgs:append-entries))
  state)

(define-state-handler raft :follower (r state (rv raft/msgs:request-vote))
  state)

(define-state-handler raft :leader (r state (rv raft/msgs:request-vote))
  state)

(define-state-handler raft :candidate (r state (rv raft/msgs:request-vote))
  state)

(defmethod run ((raft raft))
  (new-heartbeat-timer raft)
  (select
    ((recv (rpc-channel (transport raft)) event)
     (apply-event raft event))
    ((recv (heartbeat-channel raft))
     (apply-event raft :heartbeat-timeout))))
