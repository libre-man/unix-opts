;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
;;;
;;; unix-opts - a minimalistic parser of command line options.
;;;
;;; Copyright (c) 2015 Mark Karpov
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

(defpackage   :unix-opts
  (:nicknames :opts)
  (:use       #:common-lisp)
  (:export    #:unknown-option
              #:missing-arg
              #:arg-parser-failed
              #:use-value
              #:skip-option
              #:reparse-arg
              #:option
              #:raw-arg
              #:define-opts
              #:argv
              #:get-opts
              #:describe)
  (:shadow    #:describe))

(in-package #:unix-opts)

(defclass option ()
  ((name
    :initarg  :name
    :accessor name
    :documentation "keyword that will be included in list returned by
GET-OPTS function if this option is given by user")
   (description
    :initarg  :description
    :accessor description
    :documentation "description of the option")
   (short
    :initarg  :short
    :accessor short
    :documentation "NIL or single char - short variant of the option")
   (long
    :initarg  :long
    :accessor long
    :documentation "NIL or string - long variant of the option")
   (arg-parser
    :initarg  :arg-parser
    :accessor arg-parser
    :documentation "if not NIL, this option requires an argument, it will be
parsed with this function")
   (meta-var
    :initarg  :meta-var
    :accessor meta-var
    :documentation "if this option requires an argument, this is how it will
be printed in option description"))
  (:documentation "representation of an option"))

(define-condition troublesome-option (simple-error)
  ((option
    :initarg :option
    :reader option))
  (:report (lambda (c s) (format s "troublesome option: ~s" (option c))))
  (:documentation "Generalization over conditions that have to do with some
particular option."))

(define-condition unknown-option (troublesome-option)
  ()
  (:report (lambda (c s) (format s "unknown option: ~s" (option c))))
  (:documentation "This condition is thrown when parser encounters
unknown (not previously defined with DEFINE-OPTS) option."))

(define-condition missing-arg (troublesome-option)
  ()
  (:report (lambda (c s) (format s "missing arg for option: ~s" (option c))))
  (:documentation "This condition is thrown when some option OPTION wants
an argument, but there is no such argument given."))

(define-condition arg-parser-failed (troublesome-option)
  ((raw-arg
    :initarg :raw-arg
    :reader raw-arg))
  (:report (lambda (c s)
             (format s
                     "argument parser failed (option: ~s, string to parse: ~s)"
                     (option c)
                     (raw-arg c))))
  (:documentation "This condition is thrown when some option OPTION wants
an argument, it's given but cannot be parsed by argument parser."))

(defparameter *options* nil
  "list of all defined options")

(defun add-option (&rest args)
  "Register an option according to ARGS."
  (let ((name        (getf args :name))
        (description (getf args :description "?"))
        (short       (getf args :short))
        (long        (getf args :long))
        (arg-parser  (getf args :arg-parser))
        (meta-var    (getf args :meta-var "ARG")))
    (unless (or short long)
      (error "at least one form of the option must be provided"))
    (check-type name        keyword)
    (check-type description string)
    (check-type short       (or null character))
    (check-type long        (or null string))
    (check-type arg-parser  (or null function))
    (check-type meta-var    string)
    (push (make-instance 'option
                         :name        name
                         :description description
                         :short       short
                         :long        long
                         :arg-parser  arg-parser
                         :meta-var    meta-var)
          *options*)))

(defmacro define-opts (&rest descriptions)
  "Define command line options. Arguments of this macro must be plists
containing various parameters. Here we enumerate all allowed parameters:

:NAME -- keyword that will be included in list returned by GET-OPTS function
if actual option is supplied by user.

:DESCRIPTION -- description of the option (it will be used in DESCRIBE
command). This argument is optional, but it's recommended to supply it.

:SHORT -- single character - short variant of the option. You may omit this
argument if you supply :LONG variant of option.

:LONG -- string, long variant of option. You may omit this argument if you
supply :SHORT variant of option.

:ARG-PARSER -- if actual option must take an argument, supply this argument,
it must be a function that takes a string and parses it.

:META-VAR -- if actual option requires an argument, this is how it will be
printed in option description."
  `(progn
     ,@(mapcar (lambda (args) (cons 'add-option args))
               descriptions)
     nil))

(defun argv ()
  "Return list of program's arguments, including command used to execute the
program as first elements of the list."
  #+abcl      ext:*command-line-argument-list*
  #+allegro   sys:command-line-arguments
  #+:ccl      ccl:*command-line-argument-list*
  #+clisp     ext:argv
  #+clozure   ccl::command-line-arguments
  #+cmu       extensions:*command-line-words*
  #+ecl       (ext:command-args)
  #+gcl       si:*command-args*
  #+lispworks system:*line-arguments-list*
  #+sbcl      sb-ext:*posix-argv*)

(defun get-opts (&optional options)
  "Parse command line options. If OPTIONS is given, it should be a list to
parse. If it's not given, the function will use ARGV function to get list of
command line arguments. Return two values: list that contains keywords
associated with command line options with DEFINE-OPTS macro, and list of
free arguments. If some option requires an argument, you can use GETF to
test presence of the option and get its argument if the option is present.

The parser may signal various conditions, let's list them all specifying
which restarts are available for every condition, and what kind of
information the programmer can extract from the conditions.

UNKNOWN-OPTION is thrown when parser encounters unknown (not previously
defined with DEFINE-OPTS) option. Use OPTION reader to get name of the
option (string). Available restarts: USE-VALUE (substitute the option and
try again), SKIP-OPTION (ignore the option).

MISSING-ARG is thrown when some option wants an argument, but there is no
such argument given. Use OPTION reader to get name of the
option (string). Available restarts: USE-VALUE (supplied value will be
used), SKIP-OPTION (ignore the option).

ARG-PARSER-FAILED is thrown when some option wants an argument, it's given
but cannot be parsed by argument parser. Use OPTION reader to get name of
the option (string) and RAW-ARG to get raw string representing the argument
before parsing. Available restarts: USE-VALUE (supplied value will be used),
SKIP-OPTION (ignore the option), REPARSE-ARG (supplied string will be parsed
instead)."
  (labels ((split-short-opts (arg)
             (if (and (> (length arg) 1)
                      (char=  #\- (char arg 0))
                      (char/= #\- (char arg 1)))
                 (mapcar (lambda (c) (format nil "-~c" c))
                         (cdr (coerce arg 'list)))
                 (list arg)))
           (split-on-= (arg)
             (if (and (> (length arg) 1)
                      (char= #\- (char arg 0))
                      (char/= #\= (char arg 1)))
                 (let ((pos (position #\= arg :test #'char=)))
                   (if pos
                       (list (subseq arg 0 pos)
                             (subseq arg (1+ pos) (length arg)))
                       (list arg)))
                 (list arg)))
           (shortp (opt)
             (and (= (length opt) 2)
                  (char=  #\- (char opt 0))))
           (longp (opt)
             (and (> (length opt) 2)
                  (char= #\- (char opt 0))
                  (char= #\- (char opt 1))))
           (optionp (str)
             (or (shortp str) (longp str)))
           (argp (str)
             (and str
                  (not (optionp str))))
           (find-option (opt)
             (multiple-value-bind (opt key)
                 (if (shortp opt)
                     (values (subseq opt 1)
                             #'short)
                     (values (subseq opt 2)
                             #'long))
               (find opt *options* :key key :test #'string=))))
    (do ((tokens (mapcan #'split-short-opts
                         (mapcan #'split-on-=
                                 (or options (cdr (argv)))))
                 (cdr tokens))
         poption-name
         poption-raw
         poption-parser
         options
         free-args)
        ((and (null tokens)
              (null poption-name))
         (values (nreverse options)
                 (nreverse free-args)))
      (labels ((push-option (name value)
                 (push name options)
                 (push value options)
                 (setf poption-name nil))
               (process-arg (arg)
                 (restart-case
                     (handler-case
                         (push-option poption-name
                                      (funcall poption-parser arg))
                       (error (condition)
                         (declare (ignore condition))
                         (error 'arg-parser-failed
                                :option poption-raw
                                :raw-arg arg)))
                   (use-value (value)
                     (push-option poption-name value))
                   (skip-option ()
                     (setf poption-name nil))
                   (reparse-arg (str)
                     (process-arg str))))
               (process-option (opt)
                 (let ((option (find-option opt)))
                   (if option
                       (let ((parser (arg-parser option)))
                         (if parser
                             (setf poption-name (name option)
                                   poption-raw  opt
                                   poption-parser parser)
                             (push-option (name option) t)))
                       (restart-case
                           (error 'unknown-option
                                  :option opt)
                         (use-value (value)
                           (process-option value))
                         (skip-option ()))))))
        (let ((item (car tokens)))
          (cond ((and poption-name (argp item))
                 (process-arg item))
                (poption-name
                 (restart-case
                     (error 'missing-arg
                            :option poption-raw)
                   (use-value (value)
                     (push-option poption-name value)
                     (when item
                       (process-option item)))
                   (skip-option ()
                     (setf poption-name nil)
                     (when item
                       (process-option item)))))
                ((optionp item)
                 (process-option item))
                (t (push item free-args))))))))

(defun describe (&key prefix suffix (stream *standard-output*))
  "Return string describing options of the program that were defined with
DEFINE-OPTS macro previously. You can supply PREFIX and SUFFIX arguments
that will be printed before and after options respectively. Output goes to
STREAM."
  (flet ((print-part (str)
           (when str
             (princ str stream)
             (terpri stream)
             (terpri stream))))
    (print-part prefix)
    (when *options*
      (format stream "Available options:~%"))
    (dolist (opt *options*)
      (with-slots (short long description arg-parser meta-var) opt
        (format stream "  ~27a~a~%"
                (concatenate
                 'string
                 (if short (format nil "-~c" short) "")
                 (if (and short long) ", " "")
                 (if long  (format nil "--~a" long) "")
                 (if arg-parser (format nil " ~a" meta-var) ""))
                description)))
    (print-part suffix)))
