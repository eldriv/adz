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

;; (def kons-handler (cmd)
;;   "Open Kons-9 inside SBCL terminal."
;;   (progn
;;     (run* cmd "nix" "develop" ".#lisp" "-c" "sbcl" "--eval" "(ql:quickload
;; :kons-9)" "--eval" "(kons-9:run)")))


;;; Inkscape

(def inkscape-handler (cmd)
  "Open Inkscape application."
  (run cmd "inkscape"))

;;; Website tools

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

;;; hello

(def greetings-user-handler (cmd)
  (let* ((user *hostname*)
         (msg (format t "Greetings, hello ~A!~%" user)))
    msg))
