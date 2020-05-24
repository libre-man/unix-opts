#!/bin/bash

if [[ -n "$COVERALLS" ]]; then
    cl -l fiveam -l cl-coveralls \
        -e '(setf fiveam:*debug-on-error* t
                 fiveam:*debug-on-failure* t)' \
        -e '(setf *debugger-hook*
                 (lambda (c h)
                   (declare (ignore c h))
                   (uiop:quit -1)))' \
        -e '(coveralls:with-coveralls (:exclude "./unix-opts-test.lisp")
             (ql:quickload :unix-opts/tests)
             (unix-opts/tests:run))'

else
    cl -l fiveam -l unix-opts \
        -e '(setf fiveam:*debug-on-error* t
                 fiveam:*debug-on-failure* t)' \
        -e '(setf *debugger-hook*
                 (lambda (c h)
                   (declare (ignore c h))
                   (uiop:quit -1)))' \
        -e '(progn
             (ql:quickload :unix-opts/tests)
             (unix-opts/tests:run))'

fi
