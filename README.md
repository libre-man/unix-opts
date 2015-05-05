# Unix-style command line options parser

This is a minimalistic parser of command line options. Main advantage of
this library is ability to concisely define command line options once and
then use this definition for parsing and extraction of command line
arguments, as well as printing description of command line options (you get
`--help` for free). This way you don't need to repeat yourself. Also,
`unix-opts` doesn't depend on anything and allows to precisely control
behavior of the parser via Common Lisp restarts.

## Installation

Copy files of this library in any place where ASDF can find them. Then you
can use it in system definitions and ASDF will take care of the rest.

Via Quicklisp:

```
(ql:quickload "unix-opts")
```

## Description

```
option condition
```

Take a condition `condition` (`unknown-option`, `missing-arg`, or
`arg-parser-failed`) and return string representing option in question.

----

```
raw-arg condition
```

Take a condition of type `arg-parser-failed` and return raw argument string.

----

```
define-opts &rest descriptions
```

Define command line options. Arguments of this macro must be plists
containing various parameters. Here we enumerate all allowed parameters:

`:name` — keyword that will be included in list returned by `get-opts`
function if actual option is supplied by user.

`:description` — description of the option (it will be used in `describe`
command). This argument is optional, but it's recommended to supply it.

`:short` — single character, short variant of the option. You may omit this
argument if you supply `:long` variant of option.

`:long` — string, long variant of option. You may omit this argument if you
supply `:short` variant of option.

`:arg-parser` — if actual option must take an argument, supply this
argument, it must be a function that takes a string and parses it.

`:meta-var` — if actual option requires an argument, this is how it will be
printed in option description.

----

```
argv
```

Return list of program's arguments, including command used to execute the
program as first elements of the list.

----

```
get-opts &optional options
```

Parse command line options. If `OPTIONS` is given, it should be a list to
parse. If it's not given, the function will use `argv` function to get list
of command line arguments. Return two values: list that contains keywords
associated with command line options with `define-opts` macro, and list of
free arguments. If some option requires an argument, you can use `getf` to
test presence of the option and get its argument if the option is present.

The parser may signal various conditions, let's list them all specifying
which restarts are available for every condition, and what kind of
information the programmer can extract from the conditions.

`unknown-option` is thrown when parser encounters unknown (not previously
defined with `define-opts`) option. Use `option` reader to get name of the
option (string). Available restarts: `use-value` (substitute the option and
try again), `skip-option` (ignore the option).

`missing-arg` is thrown when some option wants an argument, but there is no
such argument given. Use `option` reader to get name of the option
(string). Available restarts: `use-value` (supplied value will be used),
`skip-option` (ignore the option).

`arg-parser-failed` is thrown when some option wants an argument, it's given
but cannot be parsed by argument parser. Use `option` reader to get name of
the option (string) and `raw-arg` to get raw string representing the
argument before parsing. Available restarts: `use-value` (supplied value
will be used), `skip-option` (ignore the option), `reparse-arg` (supplied
string will be parsed instead).

----

```
describe &key prefix suffix stream
```

Return string describing options of the program that were defined with
`define-opts` macro previously. You can supply `prefix` and `suffix`
arguments that will be printed before and after options respectively. If
`usage-of` is supplied, it should be a string, name of the program for
"Usage: " section. This section is only printed if this name is given. If
your program takes arguments (apart from options), you can specify how to
print them in "Usage: " section with `args` option (should be a string
designator). Output goes to `stream` (default value is `*standard-output*`).

## Example

Go to `example` directory. Now, you can use `example.lisp` file to see if
`unix-opts` is cool enough for you to use. SBCL users can use `example.sh`
file. Here is some tests:

```
$ sh example.sh --help
example — program to demonstrate unix-opts library

Usage: example.sh [-h|--help] [-v|--verbose] [-l|--level LEVEL]
                  [-o|--output FILE] [FREE-ARGS]

Available options:
  -h, --help               print this help text
  -v, --verbose            verbose output
  -l, --level LEVEL        the program will run on LEVEL level
  -o, --output FILE        redirect output to file FILE

so that's how it works…
free args:
$ sh example.sh -v file1.txt file2.txt
OK, running in verbose mode…
free args: file1.txt, file2.txt
$ sh example.sh --level 10 --output foo.txt bar.txt
I see you've supplied level option, you want 10 level!
I see you want to output the stuff to "foo.txt"!
free args: bar.txt
$ sh example.sh --level kitty foo.txt
fatal: cannot parse "kitty" as argument of "--level"
free args:
$ sh example.sh --hoola-boola noola.txt
warning: "--hoola-boola" option is unknown!
free args: noola.txt
$ sh example.sh -vgl=10
warning: "-g" option is unknown!
OK, running in verbose mode…
I see you've supplied level option, you want 10 level!
free args:
```

Take a look at `example.lisp` and you will see that the library is pretty
sexy! Basically, we have defined all the options just like this:

```common-lisp
(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :verbose
   :description "verbose output"
   :short #\v
   :long "verbose")
  (:name :level
   :description "the program will run on LEVEL level"
   :short #\l
   :long "level"
   :arg-parser #'parse-integer
   :meta-var "LEVEL")
  (:name :output
   :description "redirect output to file FILE"
   :short #\o
   :long "output"
   :arg-parser #'identity
   :meta-var "FILE"))
```

## License

Copyright © 2015 Mark Karpov

Distributed under MIT License.
