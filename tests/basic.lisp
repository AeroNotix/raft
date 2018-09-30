(defpackage raft/tests/basic
  (:use :cl
        :raft
        :raft/persistence
        :raft/persistent-hash-table
        :rove)
  (:export #:run!))
(in-package :raft/tests/basic)

(defhook :before
  (uiop:delete-file-if-exists "new.lht"))

(defhook :after
  (uiop:delete-file-if-exists "new.lht"))

(defun apply-log-entries (pht log-entries)
  (loop for log-entry in log-entries
     do
       (apply-log-entry pht log-entry)))

(defmacro make-log-entry (term index name operands)
  `(make-instance 'simple-log-entry :term ,term :index ,index :operation
                  (make-instance 'simple-operation :name ,name :operands ,operands)))

(deftest serialize-and-deserialize-well-formed-single-log
  (warn "This should be in a test module specific to disk formats")
  (let ((pht (make-instance 'persistent-hash-table :path "new.lht"))
        (log-entries (list
                      (make-log-entry 0 0 :set '("X" "1"))
                      (make-log-entry 0 1 :set '("Y" "2"))
                      (make-log-entry 0 2 :set '("Z" "3")))))
    (apply-log-entries pht log-entries)
    (ok (equal (retrieve-log-entry pht "X") "1") "We set X=1, and can read it")
    (ok (equal (retrieve-log-entry pht "Y") "2") "We set Y=2, and can read it")
    (ok (equal (retrieve-log-entry pht "Z") "3") "We set Z=3, and can read it"))
  (let ((pht (make-instance 'persistent-hash-table :path "new.lht")))
    (ok (equal (retrieve-log-entry pht "X") "1") "After serialization/deserialization, reads still work")
    (ok (equal (retrieve-log-entry pht "Y") "2") "After serialization/deserialization, reads still work")
    (ok (equal (retrieve-log-entry pht "Z") "3") "After serialization/deserialization, reads still work")))

(deftest attempt-deserialize-empty-file
  (ok (make-instance 'persistent-hash-table :path "tests/empty-file")
      "Deserializing an empty file shouldn't cause errors (perhaps in future it will)"))

(deftest serialize-deserialize-set/get-operations
  (let ((pht (make-instance 'persistent-hash-table :path "new.lht"))
        (log-entries (list
                      (make-log-entry 0 0 :set '("X" "1"))
                      (make-log-entry 0 1 :del '("X")))))
    (apply-log-entries pht log-entries)
    (ok (equal (retrieve-log-entry pht "X") nil)
        "Setting a value, then deleting it, then reading back from the log, should not contain that value")))

(defun run! ()
  (rove:run :raft/tests/basic))
