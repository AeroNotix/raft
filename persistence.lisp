(defpackage :raft/persistence
  (:use :cl)
  (:export
   #:name
   #:operands
   #:index
   #:term
   #:operation
   #:persister
   #:simple-operation
   #:simple-log-entry
   #:apply-log-entry
   #:retrieve-log-entry))
(in-package :raft/persistence)


(defclass persister () ())

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
  (format stream "#<LOG-ENTRY INDEX: ~D TERM: ~D OP: ~A>"
          (raft/persistence:index le) (term le) (op le)))

(defgeneric apply-log-entry (persister log-entry)
  (:documentation "takes a log-entry and passes it to the persister
  to persist to the backend persistence layer"))

(defgeneric retrieve-log-entry (persister key)
  (:documentation "takes a key, which represents the target of a
  previous operation (as in a key:value store) and returns the value associated with it"))
