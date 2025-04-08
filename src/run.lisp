(uiop:define-package #:adz/src/run
  (:use #:cl
        #:marie
        #:adz/src/core))

(in-package #:adz/src/run)


;;; Emacs and Lisp

(def run-handler (cmd)
  "Run Emacs dev-env."
  (run* cmd :flake "nix" "develop" ".#lisp" "-c" "emacs"))

(def update-handler (cmd)
  "Update flake."
  (run* cmd :flake "nix" "flake" "update"))

(def show-handler (cmd)
  "Display error in flake."
  (run* cmd :flake "nix" "flake" "show"))

(def version-handler (cmd)
  "Check SBCL version."
  (run* cmd :flake "nix" "develop" ".#lisp" "-c" "sbcl" "--version"))

(def shell-handler (cmd)
  "Check SBCL version."
  (run* cmd :flake "nix" "develop" ".#lisp"))


;;; Kons-9
(def kons-handler (cmd)
  "Open Kons-9 inside SBCL terminal."
  (progn
    (run* cmd "nix" "develop" ".#lisp" "-c" "sbcl" "--eval" "(ql:quickload
:kons-9)" "--eval" "(kons-9:run)")))


;;; Website tools

(def krei-web-handler (cmd)
  "Initialize Krei web."
  (progn
    (run* cmd :web "hugo")
    (run* cmd :web "npm" "start" "run" "&")
    (run* cmd :web "firefox" "-new-tab" "https://localhost:1313")))

(def redmine-handler (cmd)
  (progn
    (run* cmd :redmine-docker "firefox" "-new-tab" "http://0.0.0.0/")
    (run* cmd :redmine-docker "make")))

(def kb-handler (cmd)
  (progn
    (run* cmd :kb-docker "docker-compose" "-f"
          "dev/containers/docker-compose.yml" "up" "-d")
    (run* cmd :kb-docker "docker" "exec" "wiki-app" "yarn" "dev")))

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
