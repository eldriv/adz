;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; adz.asd --- top-level ASDF file for adz

(defsystem "adz"
  :name "adz"
  :long-name "adz"
  :description ""
  :long-description ""
  :author "Eldriv"
  :maintainer "Eldriv"
  :license ""
  :homepage ""
  :bug-tracker ""
  :source-control ""
  :class :package-inferred-system
  :depends-on (#:uiop
               #:marie
               #:clingon
               #:adz/src/core
               #:adz/src/run
               #:adz/src/commands
               #:adz/src/main)
  :build-operation "program-op"
  :build-pathname "adz"
  :entry-point "adz/src/main:main")
