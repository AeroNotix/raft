(defpackage raft
  (:use :common-lisp)
  (:import-from #:raft/msgs
                #:append-entries #:append-entries-response)
  (:import-from #:raft/disk
                #:persistent-hash-table
                #:log-entry
                #:operation))
