(defpackage :raft/persistent-hash-table
  (:use :cl
        :raft/binary
        :raft/conditions
        :raft/persistence)
  (:export
   :persistent-hash-table
   :retrieve-log-entry
   :apply-log-entry))
(in-package :raft/persistent-hash-table)


(defclass persistent-hash-table (persister)
  ((path
    :initarg :path
    :accessor path)
   (stored-index
    :initarg :index
    :initform 0
    :accessor stored-index)
   (in-memory-hash-table
    :accessor imht)))

(defparameter +log-entry-header+ #(#xCD #xCD #xCD #xCD)
  "A marker for a log entry header, helpful to orientate outselves in
  a stream of log entries")

(defmethod serialize-operation ((pht persistent-hash-table) (operation simple-operation) (stream stream))
  ;; handle key/values len > #xFF
  (with-slots (name operands) operation
    (write-byte (if (eq :set name) 0 1) stream)
    (write-byte (length operands) stream)
    (when (first operands)
      (write-byte (length (first operands)) stream)
      (write-sequence (flexi-streams:string-to-octets (first operands)) stream))
    (when (second operands)
      (write-byte (length (second operands)) stream)
      (write-sequence (flexi-streams:string-to-octets (second operands)) stream))))

(defmethod deserialize-operation ((pht persistent-hash-table) (stream stream))
  (let* ((op (if (eq (read-byte stream) 0) :set :del))
         (number-of-operands (read-byte stream))
         (operands nil))
    (make-instance 'simple-operation
                   :name op
                   :operands (when (plusp number-of-operands)
                               (loop for operand below number-of-operands
                                  collect (read-sized-string-from-stream stream))))))

(defmethod advance-stream-to-log-entry ((stream stream))
  (eq (loop
         for seq = #(0 0 0 0)
         for bytes-read = (read-sequence seq stream)
         until
           (or (equalp seq +log-entry-header+)
               (= bytes-read 0))
         finally (return bytes-read))
      (length +log-entry-header+)))

(defmethod serialize-log-entry ((pht persistent-hash-table) (le simple-log-entry) (stream stream))
  (write-sequence +log-entry-header+ stream)
  (write-bigint (raft/persistence:index le) stream)
  (write-bigint (term le) stream)
  (serialize-operation pht (operation le) stream))

(defmethod deserialize-log-entry ((pht persistent-hash-table) (stream stream))
  (handler-case (when (advance-stream-to-log-entry stream)
                  (let ((raft/persistence:index (read-bigint stream))
                        (term (read-bigint stream))
                        (operation (deserialize-operation pht stream)))
                    (make-instance 'simple-log-entry
                                   :index index
                                   :term term
                                   :operation operation)))
    (end-of-file () nil)))

(defmethod apply-disk-log-entries ((pht persistent-hash-table) (stream stream))
  (file-position stream 0)
  (loop for log-entry in (sort (loop
                              for entry = (deserialize-log-entry pht stream)
                              until (not entry)
                              collect entry) #'< :key #'index)
     do
       (progn
         (apply-operation pht (operation log-entry))
         (setf (stored-index pht) (raft/persistence:index log-entry)))))

(defmethod initialize-instance :after ((pht persistent-hash-table) &key)
  (with-open-file (disk-hash-table (path pht)
                                   :element-type 'unsigned-byte
                                   :direction :io
                                   :if-does-not-exist :create
                                   :if-exists :append)
    (setf (imht pht) (make-hash-table :test 'equal))
    (apply-disk-log-entries pht disk-hash-table)
    pht))

(defmethod apply-operation ((pht persistent-hash-table) (op simple-operation))
  (if (eq (name op) :set)
      (setf (gethash (first (operands op)) (imht pht)) (second (operands op)))
      (remhash (first (operands op)) (imht pht))))

(defmethod retrieve-log-entry ((pht persistent-hash-table) key)
  (gethash key (imht pht)))

(defmethod apply-log-entry ((pht persistent-hash-table) (le simple-log-entry))
  (when (< (index le) (stored-index pht))
    (error 'log-entry-too-old
           :attempted-index (raft/persistence:index le)
           :highest-index (stored-index pht)))
  (with-open-file (disk-hash-table (path pht)
                                   :element-type 'unsigned-byte
                                   :direction :io
                                   :if-does-not-exist :create
                                   :if-exists :append)
    (serialize-log-entry pht le disk-hash-table))
  (apply-operation pht (operation le))
  (setf (stored-index pht) (raft/persistence:index le)))
