(asdf:defsystem :raft
  :class :package-inferred-system
  :version "0.0.1"
  :description "Raft consensus algorithm"
  :license "BSD"
  :depends-on (:log4cl
               :flexi-streams
               :alexandria
               :bordeaux-threads
               :place-utils
               :usocket
               :usocket-server
               :chanl
               :trivial-timers

               :raft/timers
               :raft/conditions
               :raft/peer
               :raft/state
               :raft/msgs
               :raft/peer
               :raft/fsm
               :raft/transport
               :raft/memory-transport
               :raft/trivial
               :raft/binary
               :raft/disk)
  :components ((:file "raft"))
  :serial t)
