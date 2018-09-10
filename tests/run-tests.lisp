(ql:quickload :rove)

#+sbcl
(ql:quickload :sb-cover)

#+sbcl
(progn
  (declaim (optimize sb-cover:store-coverage-data)))

(ql:quickload :raft/tests/basic)
(raft/tests/basic::run!)

#+sbcl
(progn
  (sb-cover:report "coverage/" )
  (declaim (optimize (sb-cover:store-coverage-data 0))))
