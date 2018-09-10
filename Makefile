LISP=sbcl
TEST_OPTS=--noinform --disable-debugger --load tests/run-tests.lisp --quit

test:
	${LISP} ${TEST_OPTS}
