(uiop:define-package #:ems/src/commands
  (:use #:cl 
           #:ems/src/core))

(in-package #:ems/src/commands)

(defmacro define-flake-command (name alias description handler)
  "Define a flake command with aliases prior to its handler."
  (let ((maker-name (intern (format nil "MAKE-~A-COMMAND" name))))
    `(defun ,maker-name ()
       (clingon:make-command
        :name ,name
        :aliases (list ,alias)
        :description ,description
        :handler ,handler))))

;;; Run commands

(defun run-handler (cmd)
  "Run Emacs dev-env."
  (run! cmd "nix" "develop" ".#lisp" "-c" "emacs"))

(defun update-handler (cmd)
  "Update flake."
  (run! cmd "nix" "flake" "update"))

(defun show-handler (cmd)
  "Display error in flake."
  (run! cmd "nix" "flake" "show"))

(defun sbcl-handler (cmd)
  "Check SBCL version."
  (run! cmd "nix" "develop" ".#lisp" "-c" "sbcl" "--eval" "(ql:quickload :kons-9)"))

(defun version-handler (cmd)
  "Check SBCL version."
  (run! cmd "nix" "develop" ".#lisp" "-c" "sbcl" "--version"))

(defun shell-handler (cmd)
  "Check SBCL version."
  (run! cmd "nix" "develop" ".#lisp"))

(define-flake-command "run" "rn" "Run the Emacs shell" #'run-handler)
(define-flake-command "update" "upd" "Update the Lisp nix flake" #'update-handler)
(define-flake-command "show" "sh" "Show output attribute of the Lisp flake" #'show-handler)
(define-flake-command "sbcl-version" "sv" "Check SBCL's version" #'version-handler)
(define-flake-command "sbcl" "sb" "Open SBCL" #'sbcl-handler)
(define-flake-command "shell" "sl" "Open DevShell" #'shell-handler)