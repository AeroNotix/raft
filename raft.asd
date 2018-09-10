(asdf:defsystem :raft
  :class :package-inferred-system
  :version "0.0.1"
  :description "Raft consensus algorithm"
  :license "BSD"
  :depends-on (:flexi-streams
               :alexandria
               :bordeaux-threads
               :place-utils
               :usocket
               :usocket-server

               :raft/msgs
               :raft/trivial
               :raft/binary
               :raft/disk)
  :components ((:file "package")
               (:file "transport")
               (:file "memory-transport")
               (:file "raft-state"))
  :serial t)
