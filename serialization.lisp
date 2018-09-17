(defpackage :raft/serialization
  (:use :cl)
  (:export #:serializer
           #:make-basic-serializer
           #:cl-store-serializer))
(in-package :raft/serialization)


(defclass serializer () ()
  (:documentation "base class for serializers"))

(defgeneric serialize (serializer thing stream))

(defgeneric deserialize (serialize stream))

(defclass cl-store-serializer (serializer) ()
  (:documentation "Very basic serializer which uses cl-store, please
  note that cl-store is a very inefficient format but provides simple
  APIs which map bytes to a legitimate CLOS instance easily"))

(defun make-basic-serializer ()
  (make-instance 'cl-store-serializer))

(defmethod serialize ((s cl-store-serializer) thing (stream stream))
  (declare (ignore s))
  (cl-store:store thing stream))

(defmethod deserialize ((s cl-store-serializer) (stream stream))
  (declare (ignore s))
  (cl-store:restore stream))
