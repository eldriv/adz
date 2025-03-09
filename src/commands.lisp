(uiop:define-package #:ems/src/commands
  (:use #:cl
        #:marie
        #:ems/src/core))

(in-package #:ems/src/commands)


;;; Emans and Lisp
(define-command "emacs" "e" "Run the Emacs shell" #'run-handler)
(define-command "update" "u" "Update the Lisp nix flake" #'update-handler)
(define-command "show" "s" "Show output attribute of the Lisp flake" #'show-handler)
(define-command "sbcl-version" "sv" "Check SBCL's version" #'version-handler)
(define-command "shell" "sl" "Open DevShell" #'shell-handler)

;;; Kons 9
(define-command "kons" "k9" "Run the Kons-9" #'kons-handler)

;;; Krei website
(define-command "krei-web" "kw" "Run the krei-web" #'krei-web-handler)


;; Open VScode
(define-command "vscode" "vc" "Run the VS code" #'vs-code-handler)
