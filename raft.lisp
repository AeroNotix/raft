(defpackage raft
  (:use :common-lisp)
  (:import-from #:raft/disk
                #:persistent-hash-table
                #:log-entry
                #:operation))
