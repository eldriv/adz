;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; ems.asd --- top-level ASDF file for ems

(defsystem "ems"
  :name "ems"
  :long-name "ems"
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
               #:ems/src/core
               #:ems/src/commands
               #:ems/src/main)
  :build-operation "program-op"
  :build-pathname "ems"
  :entry-point "ems/src/main:main")
