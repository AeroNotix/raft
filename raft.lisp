(defpackage raft
  (:use :common-lisp :chanl :raft/state)
  (:import-from #:raft/trivial
                #:while)
  (:import-from #:raft/disk
                #:persistent-hash-table
                #:log-entry
                #:operation)
  (:import-from #:raft/state
                #:current-term)
  (:import-from #:raft/transport
                #:rpc-channel)
  (:import-from #:raft/memory-transport
                #:memory-transport)
  (:import-from #:raft/transport
                #:rpc-channel
                #:request-vote)
  (:import-from #:raft/fsm
                #:define-state-machine
                #:define-state-handler
                #:apply-event
                #:state))
(in-package :raft)


(define-state-machine raft :follower (raft-state)
  ((transport
    :initarg :transport
    :initform nil
    :accessor transport)
   (server-id
    :initarg :server-id
    :initform nil
    :accessor server-id)
   (servers
    :initarg :servers
    :initform nil
    :accessor servers)
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
    :accessor internal-raft-state)))

(defmethod raft/fsm:state ((raft raft))
  (current-state raft))

(defmethod (setf raft/fsm:state) (state (raft raft))
  (setf (current-state raft) state))

(defmethod reset-heartbeat-timer ((raft raft))
  (when (and (heartbeat-timer raft)
             (heartbeat-channel raft))
    ;; todo: wrap the timer type, present it as an opaque object
    (chanl:recv (heartbeat-channel raft) :blockp nil)
    (trivial-timers:unschedule-timer (heartbeat-timer raft))))

(defmethod new-heartbeat-timer ((raft raft))
  (reset-heartbeat-timer raft)
  (multiple-value-bind (hb-channel hb-timer) (raft/timers:after (1+ (random 3)))
    (setf (heartbeat-timer raft) hb-timer)
    (setf (heartbeat-channel raft) hb-channel)))

(defmethod request-votes ((raft raft))
  (log:debug "~A requesting votes" raft)
  (let* ((rs (internal-raft-state raft))
         (rv (make-instance 'raft/msgs:request-vote
                            :last-log-term (last-log-term rs)
                            :last-log-index (last-log-index rs)
                            :candidate-id (server-id raft))))
    (loop for peer in (servers raft)
       do
         (request-vote (transport raft) peer rv))))

(defmethod start-leader-election ((raft raft))
  (log:warn "Starting leader election: ~A" raft)
  (incf (current-term (internal-raft-state raft)))
  (setf (votes raft) 0)
  (request-votes raft))

(defmethod cease-leader-election ((raft raft) (rv raft/msgs:request-vote-response))
  (setf (current-term (internal-raft-state raft)) (term rv)))

(defmethod become-leader ((raft raft))
  (log:debug "Raft instance ~A becoming leader" raft)
  (setf (leader raft) (local-address (transport raft))))

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
  (let ((rvr (make-instance 'raft/msgs:request-vote-response
                            :successful-p (> (term rv) (current-term (internal-raft-state r)))
                            :term (current-term (internal-raft-state r)))))
    (return :candidate)

    state))

(define-state-handler raft :candidate (r state (rv raft/msgs:request-vote-response))
  (log:debug "Candidate ~A received vote response ~A" r rv)
  (when (and (eq (current-term (internal-raft-state r)) (vote-granted rv)))
    (incf (votes r)))
  (when (> (term rv) (current-term (internal-raft-state r)))
    (cease-leader-election r rv)
    (return :follower))
  (when (>= (votes r) (quorum r))
    (become-leader r)
    (return :leader)))

(defmethod run ((raft raft))
  (new-heartbeat-timer raft)
  (let* ((exit-channel (make-instance 'chanl:channel))
         (raft-thread (bt:make-thread
                       (lambda ()
                         (while (not (recv exit-channel :blockp nil))
                           (select
                             ((recv (rpc-channel (transport raft)) event)
                              (apply-event raft event))
                             ((recv (heartbeat-channel raft))
                              (apply-event raft :heartbeat-timeout))
                             (t
                              ;; yeah, oh yeah, you fucking like that?
                              ;; chanl is garbage. It spins the
                              ;; processor in a select
                              (sleep 0.1)))))
                       :name (format nil "RAFT-INSTANCE: ~A" (server-id raft)))))
    (values exit-channel raft-thread)))
