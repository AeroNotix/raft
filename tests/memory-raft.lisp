(defpackage raft/tests/memory-raft
  (:use :cl
        :raft
        :rove)
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


(deftest simple-leadership-election
    (let ((rafts (make-n-rafts 5)))
      (ok (eq (length rafts) 5))))

(defun run! ()
  (rove:run :raft/tests/memory-raft))
