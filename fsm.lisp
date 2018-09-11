(defpackage :raft/fsm
  (:use :cl)
  (:export
   #:apply-event
   #:define-state-machine
   #:define-state-handler
   #:state-machine
   #:state))
(in-package :raft/fsm)


(defclass finite-state-machine ()
  ((state
    :initarg :state
    :initform :default
    :accessor state
    :documentation "The current state of the state-machine.")))

(defgeneric state-machine-event (state-machine state-machine-state event))

(defmethod state-machine-event :around (state-machine state-machine-state event)
  (log:debug "State machine: ~A in state: ~A handling event: ~A" state-machine state-machine state event))

(defun apply-event (state-machine event)
  (multiple-value-bind (next-state recur-p)
      (state-machine-event state-machine (state state-machine) event)
    (setf (state state-machine) next-state)
    (when recur-p
      (apply-event state-machine event))))

(defmacro define-state-machine (name initial-state superclasses slots)
  `(progn
     (defclass ,name ,(append (list 'finite-state-machine) superclasses)
       ,slots)
     (defmethod initialize-instance :after ((fsm ,name) &key)
       (setf (state fsm) ,initial-state))))

(defmacro define-state-handler (state-machine-type state-machine-state
                                (state-machine-symbol state-symbol event-specialiser)
                                &body body)
  (let ((symb (gensym)))
    `(defmethod state-machine-event ((,state-machine-symbol ,state-machine-type)
                                     (,state-symbol (eql ,state-machine-state))
                                     ,(if (consp event-specialiser)
                                          event-specialiser
                                          `(,symb (eql ,event-specialiser))))
       ,@body)))
