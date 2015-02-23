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
  (:export    #:define
              #:argv
              #:get
              #:describe)
  (:shadow    #:get
              #:describe))

(in-package #:unix-opts)

(defmacro define (&rest descriptions)
  "Define command line options. DESCRIPTIONS must be a list, where every
element is also a list. CAR of this list must be a keyword denoting name of
the option. CADR of this list must be a string describing the option. Next,
there may be a character (short option) and/or string (long option). If
particular option should take an argument, you may add a function that will
be called to parse the argument in form of string into your desired
type. This library provides basic parsers, but you can write and use your
own."
  (declare (ignore descriptions)))

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

(defun get (&optional options)
  "Parse command line options. If OPTIONS is given, it should be a list to
parse. If it's not given, the function will use ARGV function to get list of
command line arguments. Return two values: list that contains keywords
associated with command line options with DEFINE-OPTS macro, and list of
free arguments. If some option requires an argument, you can use GETF to
test presence of the option and get its argument if the option is present."
  (declare (ignore options)))

(defun describe (&key alpha-sort)
  "Return string describing options of the program that were defined with
DEFINE macro previously. If argument ALPHA-SORT is given and it's not NIL,
descriptions of the options will be sorted alphabetically."
  (declare (ignore alpha-sort)))
