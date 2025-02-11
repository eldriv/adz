(uiop:define-package #:ems/src/commands
  (:use #:cl 
           #:ems/src/core))

(in-package #:ems/src/commands)

(define-flake-command "run" "rn" "Run the Emacs shell" #'run-handler)
(define-flake-command "update" "upd" "Update the Lisp nix flake" #'update-handler)
(define-flake-command "show" "sh" "Show output attribute of the Lisp flake" #'show-handler)
(define-flake-command "sbcl-version" "sv" "Check SBCL's version" #'version-handler)
(define-flake-command "sbcl" "sb" "Open SBCL" #'sbcl-handler)
(define-flake-command "shell" "sl" "Open DevShell" #'shell-handler)