#!/bin/bash

if [[ -n "$COVERALLS" ]]; then
    cl -l unix-opts -l unix-opts/tests -l cl-coveralls \
        -e '(setf *debugger-hook*
                 (lambda (c h)
                   (declare (ignore c h))
                   (uiop:quit 1)))' \
        -e '(coveralls:with-coveralls (:exclude "./unix-opts-test.lisp")
             (unix-opts/tests:run))'

else
    cl -l unix-opts -l unix-opts/tests \
        -e '(progn
              (uiop:quit (if (unix-opts/tests:run) 0 1)))'

fi
