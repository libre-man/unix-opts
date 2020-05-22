;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; This is some tests for Unix-opts library.
;;;
;;; Copyright © 2015–2018 Mark Karpov
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :unix-opts/tests
  (:shadowing-import-from :cl :describe)
  (:use :cl :unix-opts)
  (:export :run))

(in-package :unix-opts/tests)

(opts:define-opts
  (:name :grab-int
         :description "grab integer INT"
         :short #\i
         :long "grab-int"
         :required t
         :arg-parser #'parse-integer
         :meta-var "INT")
  (:name :grab-str
         :description "grab string STR"
         :short #\s
         :long "grab-str"
         :arg-parser #'identity
         :meta-var "STR")
  (:name :grab-string
         :description "option to cause ambiguity with grab-str as well as help test the print function of opts:describe due to this long description"
         :short #\s
         :long "grab-string"
         :arg-parser #'identity)
  (:name :flag-a
         :description "flag with short form only"
         :short #\a)
  (:name :flag-b
         :description "flag with long form only"
         :long "flag-b"))

;;; Here is some variables that we will use and functions to reset them.

(defparameter *unknown-options* nil "We collect all unknown options here.")
(defparameter *ambiguous-options* nil "We collect all ambiguous options here.")
(defparameter *missing-arg-options* nil
  "Options that need an argument, but don't get one.")
(defparameter *malformed-arguments* nil "Here we collect malformed arguments.")
(defparameter *missing-required-options*
  "Here we collect missing required options.")

(defun reset-state ()
  "Reset some special variables that are used to collect data about some
aspects of the tests."
  (setf *unknown-options*          nil
        *missing-arg-options*      nil
        *missing-required-options* nil
        *malformed-arguments*      nil))

(defun finish-collecting ()
  "Call this after parsing."
  (setf *unknown-options*     (nreverse *unknown-options*)
        *missing-required-options* (nreverse *missing-required-options*)
        *missing-arg-options* (nreverse *missing-arg-options*)
        *malformed-arguments* (nreverse *malformed-arguments*)))

;;; The tests themselves.

(defun parse-opts (opts &key unknown-option missing-arg arg-parser-failed
                          missing-required ambiguous-option)
  "Parse OPTS, return results and collect some data in special variables.
Keyword arguments allow to set arguments for `invoke-restart' function. It's
recommended to supply them all if you don't want to end in the debugger."
  (reset-state)
  (multiple-value-prog1
      (handler-bind
          ((unknown-option
            (lambda (c)
              (push (option c) *unknown-options*)
              (when unknown-option
                (apply #'invoke-restart unknown-option))))
           (ambiguous-option
            (lambda (c)
              (push (option c) *ambiguous-options*)
              (when ambiguous-option
                (apply #'invoke-restart ambiguous-option))))
           (missing-arg
            (lambda (c)
              (push (option c) *missing-arg-options*)
              (when missing-arg
                (apply #'invoke-restart missing-arg))))
           (missing-required-option
            (lambda (c)
              ;; TODO: Should we export unix-opts::name?
              (push (mapcar #'unix-opts::name (missing-options c)) *missing-required-options*)
              (when missing-required
                (apply #'invoke-restart missing-required))))
           (arg-parser-failed
            (lambda (c)
              (push (raw-arg c) *malformed-arguments*)
              (when arg-parser-failed
                (apply #'invoke-restart arg-parser-failed)))))
        (get-opts opts))
    (finish-collecting)))

(defun argv-test ()
  (assert (typep (opts:argv) 'list)))

(defun unexpected-options-test ()
  (assert (typep (handler-case (opts:get-opts '("--grab-int" "10" "--rere"))
                   (condition (c) c))
                 'unknown-option))
  (assert (typep (handler-case (opts:get-opts '())
                   (condition (c) c))
                 'missing-required-option))
  (assert (typep (handler-case (opts:get-opts '("--grab-int" "hello"))
                   (condition (c) c))
                 'arg-parser-failed))
  (assert (typep (handler-case (opts:get-opts '("--grab-int"))
                   (condition (c) c))
                 'missing-arg))
  ;; TODO: Should we error in the following case?
  (assert (typep (handler-case (opts:get-opts '("--grab-int" "10" "-a" "11"))
                   (condition (c) c))
                 'list)))

(defun miscelleneous-1 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-int" "10" "--rere" "11" "-s" "-a" "foo.txt")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-int 10 :flag-a t)))
    (assert (equalp free-args '("11" "foo.txt")))
    (assert (equalp *unknown-options* '("--rere")))
    (assert (equalp *missing-arg-options* '("-s")))
    (assert (equalp *malformed-arguments* nil))))

(defun miscelleneous-2 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("-asri=13" "--flag-b" "--flag-b" "foo.txt" "bar.txt")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:flag-a t :grab-int 13 :flag-b t :flag-b t)))
    (assert (equalp free-args '("foo.txt" "bar.txt")))
    (assert (equalp *unknown-options* '("-r")))
    (assert (equalp *missing-arg-options* '("-s")))
    (assert (equalp *malformed-arguments* nil))))

(defun miscelleneous-3 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-str=fooba" "-i" "what" "-i" "100" "--roro" "-")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-str "fooba" :grab-int 100)))
    (assert (equalp free-args '("-")))
    (assert (equalp *unknown-options* '("--roro")))
    (assert (equalp *missing-arg-options* nil))
    (assert (equalp *malformed-arguments* '("what")))))

(defun miscelleneous-4 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("--foobar" "cat" "-sl") ; very tricky (see restarts)
                  :unknown-option    '(use-value "--grab-int")
                  :missing-arg       '(use-value "my-string")
                  :arg-parser-failed '(reparse-arg "15"))
    (assert (equalp options '(:grab-int 15 :grab-str "my-string"
                          :grab-int "my-string")))
    (assert (equalp free-args nil))
    (assert (equalp *unknown-options* '("--foobar" "-l")))
    (assert (equalp *missing-arg-options* '("-s" "--grab-int")))
    (assert (equalp *malformed-arguments* '("cat")))))

(defun miscelleneous-5 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-int" "10" "--grab" "14" "--grab-s")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option)
                  :ambiguous-option  '(skip-option))
    (assert (equalp options '(:grab-int 10)))
    (assert (equalp free-args '("14")))
    (assert (null (set-difference *ambiguous-options* '("--grab" "--grab-s")
                              :test #'equal)))
    (assert (equalp *malformed-arguments* nil))))

(defun miscelleneous-6 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-int" "15" "--" "--grab-int" "16")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-int 15)))
    (assert (equalp free-args '("--grab-int" "16")))
    (assert (equalp *unknown-options* nil))
    (assert (equalp *missing-arg-options* nil))
    (assert (equalp *malformed-arguments* nil))))

(defun miscelleneous-7 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("-s" "5")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :missing-required  '(skip-option)
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-str "5")))
    (assert (equalp free-args '()))
    (assert (equalp *missing-required-options* '((:grab-int))))
    (assert (equalp *unknown-options* nil))
    (assert (equalp *missing-arg-options* nil))
    (assert (equalp *malformed-arguments* nil))))

(defun miscelleneous-8 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("-s" "5")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :missing-required  '(use-value (15))
                  :arg-parser-failed '(skip-option))
    (assert (equalp options '(:grab-str "5" :grab-int 15)))
    (assert (equalp free-args '()))
    (assert (equalp *missing-required-options* '((:grab-int))))
    (assert (equalp *unknown-options* nil))
    (assert (equalp *missing-arg-options* nil))
    (assert (equalp *malformed-arguments* nil))))

(defun expand-opts-test ()
  (assert (equalp
           (macroexpand-1
            '(opts:expand-opts
              (:help "Show this help text.")
              (:port "Port number on which to run the server." #'parse-integer t)
              (:swank-port "Port number at which to start swank [default: 8080]" #'parse-integer)
              (:debug "Run in debug mode if specified" #'identity)))
           '(UNIX-OPTS:DEFINE-OPTS
             (:NAME :HELP :DESCRIPTION "Show this help text." :SHORT #\h :REQUIRED NIL
              :LONG "help" :ARG-PARSER NIL)
             (:NAME :PORT :DESCRIPTION "Port number on which to run the server." :SHORT
              #\p :REQUIRED T :LONG "port" :ARG-PARSER #'PARSE-INTEGER)
             (:NAME :SWANK-PORT :DESCRIPTION
              "Port number at which to start swank [default: 8080]" :SHORT #\s :REQUIRED
              NIL :LONG "swank-port" :ARG-PARSER #'PARSE-INTEGER)
             (:NAME :DEBUG :DESCRIPTION "Run in debug mode if specified" :SHORT #\d
              :REQUIRED NIL :LONG "debug" :ARG-PARSER #'IDENTITY)))))

(defun run ()
  (dolist (fn '(argv-test unexpected-options-test
                miscelleneous-1
                miscelleneous-2
                miscelleneous-3
                miscelleneous-4
                miscelleneous-5
                miscelleneous-6
                miscelleneous-7
                miscelleneous-8
                expand-opts-test))
    (funcall fn)
    (format t "~D ran successfully~%" fn))
  (format t "All tests ran successfully!"))
