(defpackage raft/tests/memory-raft
  (:use :cl
        :raft
        :rove
        :raft/trivial)
  (:export #:run!))
(in-package :raft/tests/memory-raft)

(defhook :before
  (log:config :warn)
  (setf raft/memory-transport::*memory-transport-directory* (make-hash-table :test 'equal)))

(defun make-n-uuids (n)
  (loop for i below n
     collect (uuid:make-v4-uuid)))

(defun make-n-rafts (n)
  (let ((uuids (make-n-uuids n)))
    (loop for uuid in uuids
                    collect
         (make-raft-instance uuid
                             'raft/memory-transport:memory-transport
                             ;; this needs to be initialized properly (path name etc)
                             'raft/persistent-hash-table:persistent-hash-table
                             'raft/memory-transport:memory-discoverer))))

(defun create-in-memory-cluster (cluster-size)
  (let* ((rafts (make-n-rafts cluster-size)))
    (mapcar #'raft::connect-to-peers rafts)
    rafts))

(deftest simple-peering
  (let* ((cluster-size 5)
         (rafts (create-in-memory-cluster cluster-size))
         (validate-peer-count (compose
                               (lambda (n) (eq n cluster-size))
                               #'hash-table-count
                               #'raft/memory-transport:peers
                               #'raft:transport))
         (hangup-transport (compose
                            #'raft/transport:hangup
                            #'raft:transport)))
    (ok (eq (length rafts) cluster-size) "Cluster size is as expected.")
    ;; XXX: checking implementation details
    (ok (eq (hash-table-count raft/memory-transport::*memory-transport-directory*) cluster-size)
        "Memory transports register themselves with the memory transport directory.")
    (ok (every #'identity (mapcar validate-peer-count rafts))
        "Every raft instance in the peer group knows about all the other peers.")
    (mapcar hangup-transport rafts)))

(deftest followers-become-candidates-after-heartbeat-timeout
  (let ((raft (first (make-n-rafts 1))))
    (ok (raft:follower-p raft)
        "All raft instances start in the follower state.")
    (raft/fsm:apply-event raft :heartbeat-timeout)
    (ok (raft:candidate-p raft) "After a heartbeat timeout all followers become candidates.")))

(deftest candidate-reverts-to-follower-upon-newer-term
  (let ((raft (first (make-n-rafts 1))))
    (ok (raft:follower-p raft)
            "All raft instances start in the follower state.")
    (raft/fsm:apply-event raft :heartbeat-timeout)
    (ok (raft:candidate-p raft)
        "After a heartbeat timeout all followers become candidates.")
    (raft/fsm:apply-event raft (make-instance 'raft/msgs:request-vote
                                              :last-log-term 1
                                              :last-log-index 1
                                              :candidate-id (make-instance 'raft/peer:peer
                                                                           :server-address :something
                                                                           :server-id :something)
                                              :term 10))
    (ok (raft:follower-p raft)
        "Candidates revert to follower when receiving a request vote with a higher term.")))

(deftest followers-reject-append-entries-with-old-terms
  (let ((raft (first (make-n-rafts 1))))
    (ok (raft:follower-p raft)
        "All raft instances start in the follower state.")
    ;; -1 chosen because it's the first lowest impossible value that
    ;; should trigger rejection of AppendEntries RPC requests.
    (raft/fsm:apply-event raft (make-instance 'raft/msgs:append-entries
                                              :leader-commit -1
                                              :entries nil
                                              :prev-log-term -1
                                              :prev-log-index -1
                                              :leader-id :test-append-entries
                                              :term -1))))

(deftest simple-leader-election
  (let* ((class-is (lambda (c)
                     (lambda (inst)
                       (log:debug "The actual class is: ~A" (class-of inst))
                       (eq (class-of inst) (find-class c)))))
         (get-rpc-channel (compose
                           #'raft/transport:rpc-channel
                           #'raft:transport))
         (recv-rpc-channel (compose
                            #'chanl:recv
                            get-rpc-channel))
         (hangup-transport (compose
                            #'raft/transport:hangup
                            #'raft:transport))
         (cluster-size 5)
         (rafts (create-in-memory-cluster cluster-size))
         (leader (first rafts))
         (followers (rest rafts)))
    (ok (eq (length rafts) cluster-size) "Cluster size is as expected.")
    (ok (eq (length (raft/discovery:find-peers (raft:discoverer leader))) cluster-size)
        "Memory transports register themselves with the memory transport directory.")
    (raft/fsm:apply-event leader :heartbeat-timeout)
    (ok (raft:candidate-p leader)
        "After a heartbeat timeout, the raft instance should convert to candidate.")
    (ok (eq (raft:votes leader) 1)
        "Starting an election leads to each candidate receiving a single vote, from themselves.")
    (let ((request-votes (mapcar recv-rpc-channel followers))
          (rvr (make-instance 'raft/msgs:request-vote-response :vote-granted t :term 1)))
      (ok (eq (length request-votes) (length followers))
          "The leader should send each intended follower a RequestVote RPC.")
      (ok (every (funcall class-is 'raft/msgs:request-vote) request-votes)
          "The only RPC in each intended follower should be a RequestVote from the intended leader.")
      (loop for follower in followers
         do
           (raft/fsm:apply-event leader rvr))
      (ok (raft:leader-p leader)
          "After all followers have replied with a successful RequestVotes response, we are the leader.")
      (ok (eq (raft:votes leader) (1+ (length followers)))
          "After all replies, the leader should have received votes from all other nodes, including themselves.")
      (ok (every #'raft:follower-p followers)
          "After a leader is chosen, all other raft instances are still followers."))
    (mapcar hangup-transport rafts)))

(deftest competing-leader-elections
  (let* ((cluster-size 7)
         (rafts (create-in-memory-cluster cluster-size))
         (leader0 (first rafts))
         (leader1 (second rafts))
         (followers (cddr rafts))
         (get-rpc-channel (compose
                           #'raft/transport:rpc-channel
                           #'raft:transport))
         (recv-rpc-channel (compose
                            #'chanl:recv
                            get-rpc-channel)))

    (raft/fsm:apply-event leader0 :heartbeat-timeout)
    (raft/fsm:apply-event leader1 :heartbeat-timeout)
    (ok (and (raft:candidate-p leader0)
             (raft:candidate-p leader1))
        "Two potential candidates running at the same time is very much allowed.")
    (ok (and (eq (raft:votes leader0) 1)
             (eq (raft:votes leader1) 1))
        "Starting an election leads to each candidate receiving a single vote, from themselves.")
    (mapcar #'raft:process-rpc-events followers)
    (raft:process-rpc-events leader0)
    (raft:process-rpc-events leader1)
    (ok (or (raft:leader-p leader0)
            (raft:leader-p leader1))
        "One of the candidates should become a leader.")
    (ok (not (and (raft:leader-p leader0)
                  (raft:leader-p leader1)))
        "But not both!")))

(deftest followers-only-vote-once
  (let* ((cluster-size 7)
         (rafts (create-in-memory-cluster cluster-size))
         (follower0 (first rafts))
         (leaders (rest rafts)))
    (mapcar (lambda (leader)
              (raft/fsm:apply-event leader :heartbeat-timeout))
            leaders)
    (raft:process-rpc-events follower0)
    (mapcar #'raft:process-rpc-events leaders)
    (ok (eq (length (remove-if-not (lambda (leader) (eq (raft:votes leader) 2)) leaders)) 1)
        "Only one leader will have two votes.")
    (ok (eq (length (remove-if-not (lambda (leader) (eq (raft:votes leader) 1)) leaders)) (- cluster-size 2))
        "Only one leader will have two votes.")))

(defun run! ()
  (rove:run :raft/tests/memory-raft))
