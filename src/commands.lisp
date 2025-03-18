(uiop:define-package #:ems/src/commands
  (:use #:cl
        #:marie
        #:ems/src/core
        #:ems/src/run))
(in-package #:ems/src/commands)

;;; Sub commands


;;; Lisp-Flakes Commands

(define-sub-command dev-emacs (e)
                    "Run the Emacs shell."
                    #'run-handler)

(define-sub-command update (u)
                    "Update the Lisp nix flake."
                    #'update-handler)

(define-sub-command show (s)
                    "Show output attribute of the Lisp flake."
                    #'show-handler)

(define-sub-command sbcl-version (sv)
                    "Check SBCL's version."
                    #'version-handler)

(define-sub-command shell (sl)
                    "Open DevShell."
                    #'shell-handler)


;;; Graphic Commands

(define-sub-command kons (k9)
                    "Run the Kons-9."
                    #'kons-handler)


;;; Web Commands

(define-sub-command krei-web (kw)
                    "Run the krei-web."
                    #'krei-web-handler
                    ", ems wt kw")

(define-sub-command wordpress-local (wl)
                    "Run the local wordpress."
                    #'wordpress-handler
                     ", ems wt wl")

(define-sub-command redmine-local (rl)
                    "Run the local redmine"
                    #'redmine-handler
                    ", ems wt rl")



;;; IDE tools command

(define-sub-command vscode (vs)
                    "Run the VS code"
                    #'vs-code-handler)

(define-sub-command non-dev-emacs (ems)
                    "Run the non-flake emacs"
                    #'emacs-handler)


;;; Rsync tools

(define-sub-command remote-local (rl)
                    "Copy files from remote to local"
                    #'rsync-handler
                    "<remote> <local>")

(define-sub-command local-remote (lr)
                    "Copy files from local to remote"
                    #'rev-rsync-handler
                    "<local> <remote>")

(define-sub-command rtar (rt)
                    "Copy files from source to your desire path."
                    #'rsync-tar-handler
                    "<source> <destination>")


;;; top-level sub-command

(def make-lisp-flakes-commands ()
  "List of Lisp-Flakes commands."
  (list
   (make-dev-emacs-command)
   (make-update-command)
   (make-show-command)
   (make-sbcl-version-command)
   (make-shell-command)))

(def make-graphic-commands ()
  "List of Graphic commands."
  (list
   (make-kons-command)))

(def make-web-commands ()
  "List of Web commands."
  (list
   (make-krei-web-command)
   (make-wordpress-local-command)
   (make-redmine-local-command)))


(def make-ide-commands ()
  (list
   (make-vscode-command)
   (make-non-dev-emacs-command)))

(def make-rsync-commands ()
  (list
   (make-remote-local-command)
   (make-local-remote-command)
   (make-rtar-command)))


;;; Main commands

(define-main-commands lisp-flakes (lf)
                      "Emacs and Lisp flakes commands"
                      t
                      (make-lisp-flakes-commands)
                      "<command>")

(define-main-commands graphic (g)
                      "Graphical tools"
                      t
                      (make-graphic-commands)
                      "<command>")

(define-main-commands web-tools (wt)
                      "Website tools"
                      t
                      (make-web-commands)
                      "<command>")

(define-main-commands ide-tools (it)
                      "IDE tools"
                      t
                      (make-ide-commands)
                      "<command>")

(define-main-commands rsync (rs)
                      "Rsync commands"
                      t
                      (make-rsync-commands)
                      "<command>")

;;; top level

(def top-level-commands ()
  (list (make-lisp-flakes-command)
        (make-graphic-command)
        (make-web-tools-command)
        (make-ide-tools-command)
        (make-rsync-command)))
