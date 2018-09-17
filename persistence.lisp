(defpackage :raft/persistence
  (:use :cl)
  (:export
   #:name
   #:operands
   #:index
   #:term
   #:operation
   #:serializer
   #:simple-operation
   #:simple-log-entry
   #:serialize-operation
   #:deserialize-operation
   #:serialize-log-entry
   #:deserialize-log-entry))
(in-package :raft/persistence)


(defclass serializer () ())

(defclass simple-operation ()
  ((name
    :initarg :name
    :initform (error "Operations must have a name")
    :accessor name)
   (operands
    :initarg :operands
    :initform nil
    :accessor operands)))

(defmethod initialize-instance :after ((operation simple-operation) &key)
  (with-slots (name) operation
    (assert (or (eq :set name)
                (eq :del name))
            (name) "The operation must be one of :SET or :DEL, ~S provided" f)))

(defgeneric serialize-operation (serializer operation stream)
  (:documentation "Takes an operation and encodes it into a stream"))

(defgeneric deserialize-operation (serializer stream)
  (:documentation "Takes a stream and reads an operation from it"))

(defclass simple-log-entry ()
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
   (operation
    :initarg :operation
    :accessor operation)))

(defmethod print-object ((le simple-log-entry) stream)
  (format stream "#<LOG-ENTRY INDEX: ~D TERM: ~D OP: ~A>" (raft/persistence:index le) (term le) (op le)))

(defgeneric serialize-log-entry (serializer log-entry stream)
  (:documentation "Takes a log entry and encodes it into a stream"))

(defgeneric deserialize-log-entry (serializer stream)
  (:documentation "takes a stream and reads a log entry from it"))
