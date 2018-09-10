(defpackage raft/tests/basic
  (:use :cl :raft :raft/disk :rove))

(in-package :raft/tests/basic)

(setup
  (uiop:delete-file-if-exists "new.lht"))

(teardown
  (uiop:delete-file-if-exists "new.lht"))

(deftest serialize-and-deserialize-single-log
    (let ((pht (make-instance 'persistent-hash-table :path "new.lht"))
          (log-entries (list
                        (make-instance 'log-entry :term 0 :index 0 :op
                                       (make-instance 'operation :f :set :operand "X" :value "1"))
                        (make-instance 'log-entry :term 0 :index 1 :op
                                       (make-instance 'operation :f :set :operand "Y" :value "2"))
                        (make-instance 'log-entry :term 0 :index 2 :op
                                       (make-instance 'operation :f :set :operand "Z" :value "3")))))
      (loop for log-entry in log-entries
         do
           (apply-pending-log-entry pht log-entry))
      (ok (equal (retrieve-log-entry pht "X") "1"))
      (ok (equal (retrieve-log-entry pht "Y") "2"))
      (ok (equal (retrieve-log-entry pht "Z") "3")))
    (let ((pht (make-instance 'persistent-hash-table :path "new.lht")))
      (ok (equal (retrieve-log-entry pht "X") "1"))
      (ok (equal (retrieve-log-entry pht "Y") "2"))
      (ok (equal (retrieve-log-entry pht "Z") "3"))))

(defun run! ()
  (rove:run :raft/tests/basic))
