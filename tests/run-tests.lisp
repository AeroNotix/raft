(ql:quickload :rove)

#+sbcl
(ql:quickload :sb-cover)

#+sbcl
(progn
  (declaim (optimize sb-cover:store-coverage-data)))

(ql:quickload :raft/tests/basic)
(ql:quickload :raft/tests/memory-raft)

(raft/tests/basic:run!)
(raft/tests/memory-raft::run!)

#+sbcl
(progn
  (handler-bind ((warning #'muffle-warning))
    (sb-cover:report "coverage/" :external-format :utf-8)
    (declaim (optimize (sb-cover:store-coverage-data 0)))))
