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
  (let* ((cluster-size 5)
         (rafts (make-n-rafts cluster-size)))
    (mapcar #'raft::connect-to-peers rafts)
    rafts))

(deftest simple-peering
    (let* ((cluster-size 5)
           (rafts (create-in-memory-cluster cluster-size))
           (validate-peer-count (compose
                                 (lambda (n) (eq n cluster-size))
                                 #'hash-table-count
                                 #'raft/memory-transport::peers
                                 #'raft::transport)))
      (ok (eq (length rafts) cluster-size))
      ;; XXX: checking implementation details
      (ok (eq (hash-table-count raft/memory-transport::*memory-transport-directory*) cluster-size))
      (ok (every #'identity (mapcar validate-peer-count rafts)))))

(defun run! ()
  (rove:run :raft/tests/memory-raft))
