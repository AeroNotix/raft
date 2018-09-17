(defsystem :raft/tests
  :class :package-inferred-system
  :version "0.0.1"
  :description "Raft consensus algorithm tests"
  :license "BSD"
  :depends-on (:raft :rove)
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
