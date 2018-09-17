(defpackage raft/tests/basic
  (:use :cl
        :raft
        :raft/disk
        :raft/persistence
        :rove))

(in-package :raft/tests/basic)

(defhook :before
  (uiop:delete-file-if-exists "new.lht"))

(defhook :after
  (uiop:delete-file-if-exists "new.lht"))

(defun apply-log-entries (pht log-entries)
  (loop for log-entry in log-entries
     do
       (apply-pending-log-entry pht log-entry)))

(defmacro make-log-entry (term index name operands)
  `(make-instance 'simple-log-entry :term ,term :index ,index :operation
                  (make-instance 'simple-operation :name ,name :operands ,operands)))

(deftest serialize-and-deserialize-well-formed-single-log
    (let ((pht (make-instance 'persistent-hash-table :path "new.lht"))
          (log-entries (list
                        (make-log-entry 0 0 :set '("X" "1"))
                        (make-log-entry 0 1 :set '("Y" "2"))
                        (make-log-entry 0 2 :set '("Z" "3")))))
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
                      (make-log-entry 0 0 :set '("X" "1"))
                      (make-log-entry 0 1 :del '("X")))))
    (apply-log-entries pht log-entries)
    (ok (equal (retrieve-log-entry pht "X") nil))))

(defun run! ()
  (rove:run :raft/tests/basic))
