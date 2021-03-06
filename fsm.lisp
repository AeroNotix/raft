(defpackage :raft/fsm
  (:use :cl)
  (:export
   #:apply-event
   #:define-state-machine
   #:define-state-handler
   #:state-machine
   #:state
   #:*recur-p*))
(in-package :raft/fsm)


(defclass finite-state-machine ()
  ((state
    :initarg :state
    :initform :default
    :accessor state
    :documentation "The current state of the state-machine.")))

(defparameter *recur-p* t)

(defgeneric state-machine-event (state-machine state-machine-state event))

(defmethod state-machine-event :around (state-machine state-machine-state event)
  (call-next-method))

(defun apply-event (state-machine event)
  (multiple-value-bind (next-state recur-p)
      (state-machine-event state-machine (state state-machine) event)
    (setf (state state-machine) next-state)
    (when (eq recur-p *recur-p*)
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
  (let* ((symb (gensym))
         (ignore-form (list state-machine-symbol state-symbol))
         (ignore-form (cons 'ignorable (if (consp event-specialiser)
                                            ignore-form
                                            (cons symb ignore-form)))))
    `(defmethod state-machine-event ((,state-machine-symbol ,state-machine-type)
                                     (,state-symbol (eql ,state-machine-state))
                                     ,(if (consp event-specialiser)
                                          event-specialiser
                                          `(,symb (eql ,event-specialiser))))
       (declare ,ignore-form)
       (block nil ,@body))))
