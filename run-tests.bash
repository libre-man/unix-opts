#!/bin/bash

if [[ -n "$COVERALLS" ]]; then
    ros \
        -e '(progn
              (ql:quickload :cl-coveralls)
              (ql:quickload :unix-opts/tests)
              (let (suc)
                (coveralls:with-coveralls ()
                  (setf suc (unix-opts/tests:run))))
              (uiop:quit (if suc 0 1)))'

else
    ros \
        -e '(progn
              (ql:quickload :unix-opts/tests)
              (uiop:quit (if (unix-opts/tests:run) 0 1)))'

fi
