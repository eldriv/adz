(defsystem "ems"
  :name "ems"
  :version "1.0.0"
  :author "Eldriv"
  :license ""
  :description "CLI tool for managing Lisp nix flake in Emacs"
  :depends-on (:clingon :uiop)
  :components ((:module "src"
                :components ((:file "ems"))))
  :build-operation "program-op"
  :build-pathname "ems"
  :entry-point "ems:main")
