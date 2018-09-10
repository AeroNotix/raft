(defpackage raft/tests/basic
  (:use :cl :raft :raft/disk :rove))

(in-package :raft/tests/basic)

(defhook :before
  (uiop:delete-file-if-exists "new.lht"))

(defhook :after
  (uiop:delete-file-if-exists "new.lht"))

(defun apply-log-entries (pht log-entries)
  (loop for log-entry in log-entries
     do
       (apply-pending-log-entry pht log-entry)))

(deftest serialize-and-deserialize-well-formed-single-log
    (let ((pht (make-instance 'persistent-hash-table :path "new.lht"))
          (log-entries (list
                        (make-instance 'log-entry :term 0 :index 0 :op
                                       (make-instance 'operation :f :set :operand "X" :value "1"))
                        (make-instance 'log-entry :term 0 :index 1 :op
                                       (make-instance 'operation :f :set :operand "Y" :value "2"))
                        (make-instance 'log-entry :term 0 :index 2 :op
                                       (make-instance 'operation :f :set :operand "Z" :value "3")))))
      (apply-log-entries pht log-entries)
      (ok (equal (retrieve-log-entry pht "X") "1"))
      (ok (equal (retrieve-log-entry pht "Y") "2"))
      (ok (equal (retrieve-log-entry pht "Z") "3")))
    (let ((pht (make-instance 'persistent-hash-table :path "new.lht")))
      (ok (equal (retrieve-log-entry pht "X") "1"))
      (ok (equal (retrieve-log-entry pht "Y") "2"))
      (ok (equal (retrieve-log-entry pht "Z") "3"))))

(deftest attempt-deserialize-empty-file
  (ok (make-instance 'persistent-hash-table :path "tests/empty-file")))

(deftest serialize-deserialize-set/get-operations
  (let ((pht (make-instance 'persistent-hash-table :path "new.lht"))
        (log-entries (list
                      (make-instance 'log-entry :term 0 :index 0 :op
                                     (make-instance 'operation :f :set :operand "X" :value "1"))
                      (make-instance 'log-entry :term 0 :index 1 :op
                                     (make-instance 'operation :f :del :operand "X")))))
    (apply-log-entries pht log-entries)
    (ok (equal (retrieve-log-entry pht "X") nil))))

(defun run! ()
  (rove:run :raft/tests/basic))
