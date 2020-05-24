#!/bin/bash

if [[ -n "$COVERALLS" ]]; then
    cl -l unix-opts/tests -e '(values)'
    cl -l unix-opts/tests -l cl-coveralls \
        -e '(progn
              (ql:quickload :cl-coveralls)
              (coveralls:with-coveralls ()
                (unix-opts/tests:run)))'

else
    cl -l unix-opts/tests \
        -e '(progn
              (uiop:quit (if (unix-opts/tests:run) 0 1)))'

fi
