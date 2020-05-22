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
  (:use :cl :fiveam :unix-opts)
  (:local-nicknames (:opts :unix-opts)))

(in-package :unix-opts/tests)

(def-suite :unix-opts)
(in-suite :unix-opts)

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
         :description "grab string STR"
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

(def-test argv ()
  (is (typep (opts:argv) 'list)))

(def-test unexpected-options ()
  (is (typep (handler-case (opts:get-opts '("--grab-int" "10" "--rere"))
               (condition (c) c))
             'unknown-option))
  (is (typep (handler-case (opts:get-opts '())
               (condition (c) c))
             'missing-required-option))
  (is (typep (handler-case (opts:get-opts '("--grab-int" "hello"))
               (condition (c) c))
             'arg-parser-failed))
  (is (typep (handler-case (opts:get-opts '("--grab-int"))
               (condition (c) c))
             'missing-arg))
  ;; TODO: Should we error in the following case?
  (is (typep (handler-case (opts:get-opts '("--grab-int" "10" "-a" "11"))
               (condition (c) c))
             'list)))

(def-test miscelleneous-1 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-int" "10" "--rere" "11" "-s" "-a" "foo.txt")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (is (equalp options '(:grab-int 10 :flag-a t)))
    (is (equalp free-args '("11" "foo.txt")))
    (is (equalp *unknown-options* '("--rere")))
    (is (equalp *missing-arg-options* '("-s")))
    (is (equalp *malformed-arguments* nil))))

(def-test miscelleneous-2 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("-asri=13" "--flag-b" "--flag-b" "foo.txt" "bar.txt")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (is (equalp options '(:flag-a t :grab-int 13 :flag-b t :flag-b t)))
    (is (equalp free-args '("foo.txt" "bar.txt")))
    (is (equalp *unknown-options* '("-r")))
    (is (equalp *missing-arg-options* '("-s")))
    (is (equalp *malformed-arguments* nil))))

(def-test miscelleneous-3 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-str=fooba" "-i" "what" "-i" "100" "--roro" "-")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (is (equalp options '(:grab-str "fooba" :grab-int 100)))
    (is (equalp free-args '("-")))
    (is (equalp *unknown-options* '("--roro")))
    (is (equalp *missing-arg-options* nil))
    (is (equalp *malformed-arguments* '("what")))))

(def-test miscelleneous-4 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("--foobar" "cat" "-sl") ; very tricky (see restarts)
                  :unknown-option    '(use-value "--grab-int")
                  :missing-arg       '(use-value "my-string")
                  :arg-parser-failed '(reparse-arg "15"))
    (is (equalp options '(:grab-int 15 :grab-str "my-string"
                          :grab-int "my-string")))
    (is (equalp free-args nil))
    (is (equalp *unknown-options* '("--foobar" "-l")))
    (is (equalp *missing-arg-options* '("-s" "--grab-int")))
    (is (equalp *malformed-arguments* '("cat")))))

(def-test miscelleneous-5 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-int" "10" "--grab" "14" "--grab-s")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option)
                  :ambiguous-option  '(skip-option))
    (is (equalp options '(:grab-int 10)))
    (is (equalp free-args '("14")))
    (is (null (set-difference *ambiguous-options* '("--grab" "--grab-s")
                              :test #'equal)))
    (is (equalp *malformed-arguments* nil))))

(def-test miscelleneous-6 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("--grab-int" "15" "--" "--grab-int" "16")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :arg-parser-failed '(skip-option))
    (is (equalp options '(:grab-int 15)))
    (is (equalp free-args '("--grab-int" "16")))
    (is (equalp *unknown-options* nil))
    (is (equalp *missing-arg-options* nil))
    (is (equalp *malformed-arguments* nil))))

(def-test miscelleneous-7 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("-s" "5")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :missing-required  '(skip-option)
                  :arg-parser-failed '(skip-option))
    (is (equalp options '(:grab-str "5")))
    (is (equalp free-args '()))
    (is (equalp *missing-required-options* '((:grab-int))))
    (is (equalp *unknown-options* nil))
    (is (equalp *missing-arg-options* nil))
    (is (equalp *malformed-arguments* nil))))

(def-test miscelleneous-8 ()
  (multiple-value-bind (options free-args)
      (parse-opts '("-s" "5")
                  :unknown-option    '(skip-option)
                  :missing-arg       '(skip-option)
                  :missing-required  '(use-value (15))
                  :arg-parser-failed '(skip-option))
    (is (equalp options '(:grab-str "5" :grab-int 15)))
    (is (equalp free-args '()))
    (is (equalp *missing-required-options* '((:grab-int))))
    (is (equalp *unknown-options* nil))
    (is (equalp *missing-arg-options* nil))
    (is (equalp *malformed-arguments* nil))))

