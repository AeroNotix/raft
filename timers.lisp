;; This package is filled with workarounds for, frankly, subpar
;; concurrency libraries with Common Lisp.

;; Timers are dealt with synchronously, channels cannot be cancelled,
;; channel select causes spin waits the CPU. A tyre fire if there ever
;; was one.
(defpackage :raft/timers
  (:use :cl :chanl)
  (:export :after
           :stop-timer
           :reset-timer))

(in-package :raft/timers)


(defparameter *all-timers* (list))

(defclass timer ()
  ((lock
    :initform (bt:make-lock "timer")
    :accessor lock)
   (n
    :initarg :n
    :accessor n)
   (timeout-channel
    :initarg :timeout-channel
    :accessor timeout-channel)
   (timer
    :initarg :timer
    :accessor timer)))

(defun after (n)
  (log:debug "starting new timer: ~A" n)
  ;; unbounded channel because there will be at *most* one write to
  ;; the channel and we want the thread to die asap because you cannot
  ;; unschedule timers until the function they've scheduled has
  ;; finished.  TODO: make some new scheduler that doesn't have these
  ;; properties.
  (let* ((timeout-channel (make-instance 'chanl:unbounded-channel))
         (timer (sb-ext:make-timer
                 (lambda ()
                   (chanl:send timeout-channel t)
                   (log:debug "timer finished: ~A" n))
                 :name "after-channel"
                 :thread t)))
    (sb-ext:schedule-timer timer n)
    (let ((timer (make-instance 'timer
                                :n n
                                :timeout-channel timeout-channel
                                :timer timer)))
      (push timer *all-timers*)
      timer)))

(defmethod stop-timer ((timer timer))
  (bt:with-recursive-lock-held ((lock timer))
    ;; flush the channel
    (loop while (chanl:recv (timeout-channel timer) :blockp nil))
    (sb-ext:unschedule-timer (timer timer))))

(defmethod reset-timer ((timer timer))
  (stop-timer timer)
  (sb-ext:schedule-timer (timer timer) (n timer)))

(defmethod recv ((timer timer) &key (blockp t))
  (recv (timeout-channel timer)))
