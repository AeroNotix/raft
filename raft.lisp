(defpackage raft
  (:use :common-lisp :chanl :raft/state)
  (:export
   #:candidate-p
   #:follower-p
   #:leader-p
   #:make-raft-instance
   #:run-state-machine
   #:transport
   #:state-check
   #:votes
   #:process-rpc-events
   #:discoverer
   #:send-heartbeats)
  (:import-from #:raft/trivial
                #:while
                #:compose)
  (:import-from #:raft/state
                #:current-term)
  (:import-from #:raft/transport
                #:rpc-channel)
  (:import-from #:raft/transport
                #:rpc-channel
                #:request-vote)
  (:import-from #:raft/fsm
                #:define-state-machine
                #:define-state-handler
                #:apply-event
                #:state
                #:*recur-p*))
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
   (server-address
    :initarg :server-address
    :initform nil
    :accessor server-address)
   (leader
    :accessor leader)
   (votes
    :initform 0
    :accessor votes)
   (voted-in-election
    :initform 0
    :accessor voted-in-election)
   (heartbeat
    :initform nil
    :accessor heartbeat
    :documentation "A value that will determine if this raft server
has experienced a timeout from not receiving AppendEntries RPCs in a
timely manner")
   (discoverer
    :initarg :discoverer
    :initform nil
    :accessor discoverer)
   (persister
    :initarg :persister
    :initform nil
    :accessor persister)
   (shutdown-channel
    :initform nil
    :accessor shutdown-channel)
   (raft-thread
    :initform nil
    :accessor raft-thread)))

(defmethod raft/fsm:state ((raft raft))
  (current-state raft))

(defmethod (setf raft/fsm:state) (state (raft raft))
  (setf (current-state raft) state))

(defmethod connect-to-peers ((raft raft))
  (loop for server in (raft/discovery:find-peers (discoverer raft))
     do
       (raft/transport:connect (transport raft) server)))

(defmethod state-check ((raft raft) state-query)
  (eq (state raft) state-query))

(defmethod candidate-p ((raft raft))
  (state-check raft :candidate))

(defmethod follower-p ((raft raft))
  (state-check raft :follower))

(defmethod leader-p ((raft raft))
  (state-check raft :leader))

(defmethod votes-required ((raft raft))
  (floor (length (raft/discovery:find-peers (discoverer raft))) 2))

(defmethod quorum-achieved-p ((raft raft))
  (and (candidate-p raft)
       (>= (votes raft)
           (votes-required raft))))

(defmethod shutdown ((raft raft) &key (force-p nil) (force-wait 0))
  ;; TODO, clean up any transport connections/threads/locks
  (send (shutdown-channel raft) t)
  (when force-p
    (warn "Forcing threads to close is inadvisable")
    (sleep force-wait)
    (bt:destroy-thread (raft-thread raft))))

(defmethod new-heartbeat-timer ((raft raft))
  (when (heartbeat raft)
    (raft/timers:stop-timer (heartbeat raft)))
  (setf (heartbeat raft) (raft/timers:after (+ 3 (random 7)))))

(defmethod send-simple-rpc ((raft raft) method (rr raft/msgs:raft-request))
  (let ((targets (raft/discovery:find-peers (discoverer raft))))
    (loop for peer in targets
       do
         (funcall method (transport raft) peer rr))))

(defmethod request-votes ((raft raft))
  (log:debug "~A requesting votes" raft)
  (let* ((rv (make-instance 'raft/msgs:request-vote
                            :term (current-term raft)
                            :last-log-term (last-log-term raft)
                            :last-log-index (last-log-index raft)
                            :candidate-id (raft/transport:encode-peer
                                           (transport raft)
                                           (server-id raft)
                                           (server-address raft)))))
    (send-simple-rpc raft #'request-vote rv)))

(defmethod send-heartbeats ((raft raft))
  (let ((ae (make-instance 'raft/msgs:append-entries
                           :term (current-term raft)
                           :leader-id (server-id raft)
                           :prev-log-index (last-applied raft)
                           ;; not correct, prev-log-term must be the
                           ;; term of the *committed* entry. Fix me.
                           :prev-log-term (current-term raft)
                           :entries nil
                           :leader-commit (commit-index raft))))
    (send-simple-rpc raft #'raft/transport:append-entries ae)))

(defmethod begin-leader-election ((raft raft))
  (log:info "Starting leader election: ~A" raft)
  (incf (current-term raft))
  (setf (votes raft) 1)
  (setf (voted-for raft) (raft/transport:encode-peer
                          (transport raft)
                          (server-id raft)
                          (server-address raft)))
  (request-votes raft))

(defmethod cease-leader-election ((raft raft) (rv raft/msgs:request-vote-response))
  (setf (current-term raft) (raft/msgs:term rv)))

(defmethod reinitialize-per-follower-indexes ((raft raft))
  (setf (next-index raft) (make-hash-table))
  (setf (match-index raft) (make-hash-table)))

(defmethod become-leader ((raft raft))
  (log:debug "Raft instance ~A becoming leader" raft)
  (setf (leader raft) (server-id raft))
  (reinitialize-per-follower-indexes raft))

(define-state-handler raft :follower (r state :heartbeat-timeout)
  (begin-leader-election r)
  :candidate)

(define-state-handler raft :leader (r state :heartbeat-timeout)
  (send-heartbeats r)
  :leader)

(define-state-handler raft :follower (r state (ae raft/msgs:append-entries))
  (let ((aer (make-instance 'raft/msgs:append-entries-response
                            :term (current-term r)
                            :success nil)))
    (setf (raft/msgs:success aer) (< (raft/msgs:term ae) (current-term r)))
    (setf (raft/msgs:success aer) (and (= (raft/state:last-log-term r) (raft/msgs:prev-log-term ae))
                                       (= (raft/state:last-log-index r) (raft/msgs:prev-log-index ae))))
    (when (raft/msgs:success aer)
      (let ((highest-index (append-new-entries r ae)))
        (when (> (leader-commit ae) (commit-index r))
          (setf (commit-index r) (min highest-index (leader-commit ae))))))))

(define-state-handler raft :leader (r state (ae raft/msgs:append-entries))
  "If AppendEntries RPC received from new leader: convert to follower"
  (when (> (raft/msgs:term ae) (current-term r))
    (values :follower *recur-p*)))

(define-state-handler raft :candidate (r state (ae raft/msgs:append-entries))
  "If AppendEntries RPC received from new leader: convert to follower"
  (when (> (raft/msgs:term ae) (current-term r))
    (values :follower *recur-p*)))

;;; RequestVote RPC handlers

(defmethod handle-request-vote ((r raft) (rv raft/msgs:request-vote))
  (log:debug "~A ~A received request-vote ~A" (state r) r rv)
  (let* ((vote-granted-p (and (> (raft/msgs:term rv) (current-term r))
                              (not (<= (raft/msgs:term rv)
                                       (voted-in-election r)))))
         (rvr (make-instance 'raft/msgs:request-vote-response
                             :vote-granted vote-granted-p
                             :term (if vote-granted-p
                                       (raft/msgs:term rv)
                                       (current-term r)))))
    (when vote-granted-p
      (new-heartbeat-timer r)
      (setf (voted-in-election r) (raft/msgs:term rv))
      (setf (state r) :follower))
    (raft/transport:request-vote-response (transport r) (raft/msgs:candidate-id rv) rvr)
    (state r)))

(define-state-handler raft :follower (r state (rv raft/msgs:request-vote))
  (handle-request-vote r rv))

(define-state-handler raft :leader (r state (rv raft/msgs:request-vote))
  (handle-request-vote r rv))

(define-state-handler raft :candidate (r state (rv raft/msgs:request-vote))
  (handle-request-vote r rv))

(define-state-handler raft :leader (r state (rv raft/msgs:request-vote-response))
  (log:info "~A received ~A while already a leader" r rv)
  (when (and (eq (raft/msgs:term rv) (current-term r))
             (raft/msgs:vote-granted rv))
    (incf (votes r)))
  :leader)

(define-state-handler raft :candidate (r state (rv raft/msgs:request-vote-response))
  (log:debug "Candidate ~A received vote response ~A" r rv)
  (when (> (raft/msgs:term rv) (current-term r))
    (cease-leader-election r rv)
    (return :follower))
  (when (and (eq (raft/msgs:term rv) (current-term r))
             (raft/msgs:vote-granted rv))
    (incf (votes r)))
  (when (quorum-achieved-p r)
    (become-leader r)
    (return :leader))
  :candidate)

(define-state-handler raft :candidate (r state :heartbeat-timeout)
  :candidate)

(defmethod process-rpc-events ((raft raft))
  (multiple-value-bind (event channel) (recv (rpc-channel (transport raft)) :blockp nil)
    (unless (null channel)
      (apply-event raft event)
      (process-rpc-events raft))))

(defun make-raft-instance (server-id transport persister discoverer &optional serializer)
  (let ((transport (if serializer
                       (make-instance transport :server-id server-id :serializer (make-instance serializer))
                       (make-instance transport :server-id server-id))))
    (make-instance 'raft
                   :server-id server-id
                   :transport transport
                   :discoverer (make-instance discoverer)
                   :persister persister)))

(defmethod run-state-machine ((raft raft))
  (log:config :debug)
  (new-heartbeat-timer raft)
  (let* ((shutdown-channel (make-instance 'chanl:channel))
         (raft-thread (bt:make-thread
                       (lambda ()
                         (connect-to-peers raft)
                         (while (not (recv shutdown-channel :blockp nil))
                           (select
                             ((recv (rpc-channel (transport raft)) event)
                              (apply-event raft event))
                             ((recv (heartbeat raft))
                              (apply-event raft :heartbeat-timeout)
                              (new-heartbeat-timer raft))
                             (t
                              ;; yeah, oh yeah, you fucking like that?
                              ;; chanl is garbage. It spins the
                              ;; processor in a select
                              (sleep 0.1)))))
                       :name (format nil "RAFT-INSTANCE: ~A" (server-id raft)))))
    (setf (shutdown-channel raft) shutdown-channel
          (raft-thread raft) raft-thread)
    raft))
