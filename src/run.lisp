(uiop:define-package #:ems/src/run
  (:use #:cl
        #:marie
        #:ems/src/core))

(in-package #:ems/src/run)


;;; Emacs and Lisp

(def run-handler (cmd)
  "Run Emacs dev-env."
  (run-flake cmd "nix" "develop" ".#lisp" "-c" "emacs"))

(def update-handler (cmd)
  "Update flake."
  (run-flake cmd "nix" "flake" "update"))

(def show-handler (cmd)
  "Display error in flake."
  (run-flake cmd "nix" "flake" "show"))

(def version-handler (cmd)
  "Check SBCL version."
  (run-flake cmd "nix" "develop" ".#lisp" "-c" "sbcl" "--version"))

(def shell-handler (cmd)
  "Check SBCL version."
  (run-flake cmd "nix" "develop" ".#lisp"))


;;; Kons-9
(def kons-handler (cmd)
  "Open Kons-9 inside SBCL terminal."
  (progn
    (run-flake cmd "nix" "develop" ".#lisp" "-c" "sbcl" "--eval" "(ql:quickload
:kons-9)" "--eval" "(kons-9:run)")))


;;; Website tools

(def krei-web-handler (cmd)
  "Initialize Krei web."
  (progn
    (run-web cmd "hugo")
    (run-web cmd "npm" "start" "run" "&")
    (run-web cmd "firefox" "-new-tab" "https://localhost:1313")))

(def wordpress-handler (cmd)
  "Initialize Krei web."
  (progn
    (run cmd "wordpress" "-r")))


;;; IDE tools

(def vs-code-handler (cmd)
  "Initialize VS code."
  (run cmd "code" "gh/krei-systems.github.io"))

(def emacs-handler (cmd)
  "Run Emacs non-dev-env."
  (run cmd "emacs"))


;;; Rsync

(def rsync-handler (cmd remote-path main-path)
  "Copy files from remote to local
rsync -av /source/directory/ /destination/directory/
"
  (apply #'run cmd "rsync" "-avP" remote-path main-path))

(def rev-rsync-handler (cmd main-path remote-path)
  "Copy files from local to remote
rsync -av username@remote-host:/source/directory/ /local/destination/
"
  (apply #'run cmd "rsync" "-avP" main-path remote-path))

(def rsync-tar-handler (cmd)
  "Compress data during transfer."
  (let ((args (clingon:command-arguments cmd)))
    (apply #'run cmd "rsync" "-avz" args)))
