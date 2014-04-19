#!/bin/bash

# Compiles a peeps cgi lisp image and installs it into your ~/bin
# Optionally enable debugging.  If you enable debugging, the debug
# file has to be writable by apache

source environment.sh

DEBUG="nil"

if [ -n "$1" ]
then
	DEBUG="$1"
fi
sbcl \
  --eval '(require :asdf)' \
  --eval '(require :lisp-cgi-utils)' \
  --eval '(require :split-sequence)' \
  --eval '(require :cl-ppcre)' \
  --eval "(defvar *debug* $DEBUG)" \
  --eval '(load "package.lisp")' \
  --eval '(load "board.lisp")' \
  --eval '(load "ai.lisp")' \
  --eval '(load "html.lisp")' \
  --eval '(load "cgi.lisp")' \
  --eval '(in-package :twenty-forty-eight)' \
  --eval "(sb-ext:save-lisp-and-die \"$DESTINATION/$EXECUTABLE\" :executable t)"

