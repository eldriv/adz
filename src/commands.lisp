(uiop:define-package #:adz/src/commands
  (:use #:cl
        #:marie
        #:adz/src/core
        #:adz/src/run))
(in-package #:adz/src/commands)

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

(define-sub-command inkscape (ik)
  "Run the Inkscape App."
  #'inkscape-handler)


;;; Web Commands

(define-sub-command krei-web (kw)
  "Run the krei-web."
  #'krei-web-handler
  ", adz wt kw")

(define-sub-command wordpress-local (wl)
  "Run the local wordpress."
  #'wordpress-handler
  ", adz wt wl")

(define-sub-command redmine-local (rl)
  "Run the local redmine"
  #'redmine-handler
  ", adz wt rl")

(define-sub-command kb-local (kl)
  "Run the local knowledge base."
  #'kb-handler
  ", adz wt kl")


;;; IDE tools command

(define-sub-command vscode (vs)
  "Run the VS code"
  #'vs-code-handler)

(define-sub-command non-dev-emacs (adz)
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

;;; hello



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
   (make-inkscape-command)))

(def make-web-commands ()
  "List of Web commands."
  (list
   (make-wordpress-local-command)))

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

(define-sub-command hello (h)
  "hello user command"
  #'greetings-user-handler
  "")

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
  (list 
   (make-hello-command)
   (make-lisp-flakes-command)
   (make-graphic-command)
   (make-web-tools-command)
   (make-ide-tools-command)
   (make-rsync-command)))
