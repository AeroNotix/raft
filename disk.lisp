(defpackage :raft/disk
  (:use :cl :raft/binary :raft/conditions)
  (:export
   :operation
   :log-entry
   :persistent-hash-table
   :retrieve-log-entry
   :apply-pending-log-entry))
(in-package :raft/disk)


(defparameter +log-entry-header+ #(#xCD #xCD #xCD #xCD)
  "A marker for a log entry header, helpful to orientate outselves in
  a stream of log entries")

(defgeneric serialize (thing stream))

(defclass operation ()
  ((f
    :initarg :f
    :accessor f)
   (operand
    :initarg :operand
    :initform nil
    :accessor operand)
   (value
    :initarg :value
    :initform nil
    :accessor value)))

(defmethod initialize-instance :after ((operation operation) &key)
  (with-slots (f operand value) operation
    (assert (or (eq :set f)
                (eq :del f))
            (f)
            "The operation must be one of :SET or :DEL, ~S provided" f)
    ;; to do allow any key/value type
    (check-type operand string "string")
    (when (eq :set f)
      (assert
       (and (not (null operand))
            (not (null value)))
       (f operand value)
       "Set operations require non-NIL operands and non-NIL values")
      (check-type value string "string"))))

(defmethod serialize ((operation operation) (stream stream))
  ;; handle key/values len > #xFF
  (with-slots (f operand value) operation
    (write-byte (if (eq :set f) 0 1) stream)
    (write-byte (length operand) stream)
    (write-sequence (flexi-streams:string-to-octets operand) stream)
    (write-byte (length value) stream)
    (write-sequence (flexi-streams::string-to-octets value) stream)))

(defclass log-entry ()
  ((index
    :initarg :index
    :initform 0
    :accessor index
    :type integer)
   (term
    :initarg :term
    :initform 0
    :accessor term
    :type integer)
   (op
    :initarg :op
    :accessor op)))

(defmethod print-object ((le log-entry) stream)
  (format stream "#<LOG-ENTRY INDEX: ~D TERM: ~D OP: ~A>" (index le) (term le) (op le)))

(defmethod serialize ((le log-entry) (stream stream))
  (write-sequence +log-entry-header+ stream)
  (write-bigint (index le) stream)
  (write-bigint (term le) stream)
  (serialize (op le) stream))

(defclass persistent-hash-table ()
  ((path
    :initarg :path
    :accessor path)
   (index
    :initarg :index
    :initform 0
    :accessor index)
   (in-memory-hash-table
    :accessor imht)))

(defmethod read-sized-string-from-stream ((stream stream))
  (let* ((string-size (read-byte stream))
         (buffer (make-array string-size)))
    (read-sequence buffer stream)
    (flexi-streams:octets-to-string buffer)))

(defmethod deserialize-operation ((stream stream))
  (let* ((op (if (eq (read-byte stream) 0) :set :del))
         (operand (read-sized-string-from-stream stream))
         (value (when (eq op :set)
                  (read-sized-string-from-stream stream))))
    (make-instance 'operation
                   :f op
                   :operand operand
                   :value value)))

(defmethod advance-stream-to-log-entry ((stream stream))
  (eq (loop
         for seq = #(0 0 0 0)
         for bytes-read = (read-sequence seq stream)
         until
           (or (equalp seq +log-entry-header+)
               (= bytes-read 0))
         finally (return bytes-read))
      (length +log-entry-header+)))

(defmethod deserialize-log-entry ((stream stream))
  (handler-case (when (advance-stream-to-log-entry stream)
                  (let ((index (read-bigint stream))
                        (term (read-bigint stream))
                        (operation (deserialize-operation stream)))
                    (make-instance 'log-entry
                                   :index index
                                   :term term
                                   :op operation)))
    (end-of-file () nil)))

(defmethod apply-disk-log-entries ((pht persistent-hash-table) (stream stream))
  (file-position stream 0)
  (loop for log-entry in (sort (loop
                              for entry = (deserialize-log-entry stream)
                              until (not entry)
                              collect entry) #'< :key #'index)
     do
       (progn
         (apply-log-entry pht (op log-entry))
         (setf (index pht) (index log-entry)))))

(defmethod initialize-instance :after ((pht persistent-hash-table) &key)
  (with-open-file (disk-hash-table (path pht)
                                   :element-type 'unsigned-byte
                                   :direction :io
                                   :if-does-not-exist :create
                                   :if-exists :append)
    (setf (imht pht) (make-hash-table :test 'equal))
    (apply-disk-log-entries pht disk-hash-table)
    pht))

(defmethod apply-log-entry ((pht persistent-hash-table) (op operation))
  (if (eq (f op) :set)
      (setf (gethash (operand op) (imht pht)) (value op))
      (remhash (operand op) (imht pht))))

(defmethod retrieve-log-entry ((pht persistent-hash-table) key)
  (gethash key (imht pht)))

(defmethod apply-pending-log-entry ((pht persistent-hash-table) (le log-entry))
  (when (< (index le) (index pht))
    (error 'log-entry-too-old
           :attempted-index (index le)
           :highest-index (index pht)))
  (with-open-file (disk-hash-table (path pht)
                                   :element-type 'unsigned-byte
                                   :direction :io
                                   :if-does-not-exist :create
                                   :if-exists :append)
    (serialize le disk-hash-table))
  (apply-log-entry pht (op le))
  (setf (index pht) (index le)))
