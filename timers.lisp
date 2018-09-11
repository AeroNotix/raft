(defpackage :raft/timers
  (:use :cl :chanl)
  (:export :after))

(in-package :raft/timers)


(defun after (n)
  (log:debug "starting new timer: ~A" n)
  (let* ((timed-out-channel (make-instance 'chanl:channel))
         (timer (sb-ext:make-timer
                 (lambda () (chanl:send timed-out-channel t :blockp t))
                 :name "after-channel"
                 :thread t)))
    (sb-ext:schedule-timer timer n)
    (values timed-out-channel timer)))
