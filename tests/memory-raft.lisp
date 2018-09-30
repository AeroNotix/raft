(defpackage raft/tests/memory-raft
  (:use :cl
        :raft
        :rove
        :raft/trivial)
  (:export #:run!))
(in-package :raft/tests/memory-raft)

(defun make-n-uuids (n)
  (loop for i below n
     collect (uuid:make-v4-uuid)))

(defun make-n-rafts (n)
  (let ((uuids (make-n-uuids n)))
    (loop for uuid in uuids
                    collect
         (make-raft-instance uuid
                             uuids
                             'raft/memory-transport:memory-transport
                             'raft/persistent-hash-table:persistent-hash-table))))

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
    (ok (eq (length rafts) cluster-size) "Cluster size is as expected")
    ;; XXX: checking implementation details
    (ok (eq (hash-table-count raft/memory-transport::*memory-transport-directory*) cluster-size)
        "Memory transports register themselves with the memory transport directory")
    (ok (every #'identity (mapcar validate-peer-count rafts))
        "Every raft instance in the peer group knows about all the other peers")
    (mapcar hangup-transport rafts)))

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
         (intended-leader (first rafts))
         (intended-followers (rest rafts)))
    (rove:ok (eq (length rafts) cluster-size) "Cluster size is as expected")
    (rove:ok (eq (hash-table-count raft/memory-transport::*memory-transport-directory*) cluster-size)
             "Memory transports register themselves with the memory transport directory")
    (raft/fsm:apply-event intended-leader :heartbeat-timeout)
    (rove:ok (raft:candidate-p intended-leader)
             "After a heartbeat timeout, the raft instance should convert to candidate")
    (let ((request-votes (mapcar recv-rpc-channel intended-followers))
          (rvr (make-instance 'raft/msgs:request-vote-response :vote-granted t :term 1)))
      (rove:ok (eq (length request-votes) (length intended-followers))
               "The leader should send each intended follower a RequestVote RPC")
      (rove:ok (every (funcall class-is 'raft/msgs:request-vote) request-votes)
               "The only RPC in each intended follower should be a RequestVote from the intended leader")
      (loop for follower in intended-followers
         do
           (raft/fsm:apply-event intended-leader rvr))
      (rove:ok (raft:leader-p intended-leader)
               "After all followers have replied with a successful RequestVotes response, we are the leader"))
    (mapcar hangup-transport rafts)))

(defun run! ()
  (rove:run :raft/tests/memory-raft))
