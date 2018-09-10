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
  (handler-bind ((warning #'muffle-warning))
    (sb-cover:report "coverage/" :external-format :utf-8)
    (declaim (optimize (sb-cover:store-coverage-data 0)))))
