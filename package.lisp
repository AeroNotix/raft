(defpackage raft
  (:use :common-lisp)
  (:import-from #:persistence
                #:simple-log-entry
                #:simple-operation)
  (:import-from #:raft/disk
                #:persistent-hash-table))
