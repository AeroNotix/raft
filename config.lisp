(defpackage :raft/config
  (:use :cl)
  (:export #:default-transport
           #:default-serializer
           #:default-persister))
(in-package #:raft/config)

(defun get-xdg-config-dir ()
  (let ((dir (uiop:getenv "XDG_CONFIG_HOME")))
    (if (or (not dir) (string= dir ""))
        (merge-pathnames  #p".config/" (user-homedir-pathname))
        dir)))

(defun load-rc-file ()
  (let* ((xdg-config-dir (get-xdg-config-dir))
         (user-rc (probe-file (merge-pathnames (user-homedir-pathname) #p".raftrc")))
         (conf-rc (probe-file (merge-pathnames #P"raft/config" xdg-config-dir)))
         (etc-rc  (probe-file #p"/etc/raft")))
    (alexandria:when-let ((rc-file (or user-rc conf-rc etc-rc)))
      (load rc-file))))

(defparameter default-transport 'raft/memory-transport:memory-transport)
(defparameter default-serializer 'raft/serialization:cl-store-serializer)
(defparameter default-persister 'raft/persistent-hash-table:persistent-hash-table)
