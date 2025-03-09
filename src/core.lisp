(uiop:define-package #:ems/src/core
  (:use #:cl
        #:marie))

(in-package #:ems/src/core)


;;; Special config

(defp *config*
    (list
     :name "ems"
     :description "A thin wrapper for interacting with my Lisp environment and others"
     :version "1.0.0"
     :usage "[command] [options]"
     :time 4.5))

(defp *paths*
    (list
     :flake (merge-pathnames #P"myflake/" (user-homedir-pathname))
     :web (merge-pathnames #P"gh/krei-systems.github.io" (user-homedir-pathname))))


;;; Utilities

(def get-config (key)
  "Get information from the *config*."
  (getf *config* key))

(def get-paths (key)
  "Get path from the *paths*"
  (getf *paths* key))

(def- log-msg (cmd fmt &rest args)
  "Log message if verbose mode is enabled."
  (when (clingon:getopt cmd :verbose)
    (apply #'format t fmt args)))

(def- run-cmd (cmd command &rest args)
  "Run a command with logging."
  (log-msg cmd "Running command: ~A ~{~A ~}~%" command args)
  (uiop:run-program (cons command args)
                    :input :interactive
                    :output :interactive
                    :error-output :interactive))

(def- run-flake (cmd command &rest args)
  "Safely execute commands in myflake directory with logging."
  (let ((dir (namestring (get-paths :flake))))
    (log-msg cmd "Changing to directory: ~A~%" dir)
    (uiop:chdir dir)
    (apply #'run-cmd cmd command args)))

(def- run-web (cmd command &rest args)
  "Safely execute commands in krei-web directory with logging."
  (let ((dir (namestring (get-paths :web))))
    (log-msg cmd "Changing to directory: ~A~%" dir)
    (uiop:chdir dir)
    (apply #'run-cmd cmd command args)))


(def- run (cmd command &rest args)
  "Safely execute commands in current directory with logging."
  (let ((dir (namestring (uiop/os:getcwd))))
    (log-msg cmd "Changing to directory: ~A~%" dir)
    (uiop:chdir dir)
    (apply #'run-cmd cmd command args)))


(defm define-command (name alias description handler)
  "Define a flake command with aliases prior to its handler."
  (let ((maker-name (intern (format nil "MAKE-~A-COMMAND" name))))
    `(def ,maker-name ()
       (clingon:make-command
        :name ,name
        :aliases (list ,alias)
        :description ,description
        :handler ,handler))))

;;;; Run commands


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


;;; Krei Website

(def krei-web-handler (cmd)
  "Initialize Krei web."
  (progn
    (run-web cmd "hugo")
    (run-web cmd "npm" "start" "run")))


;;; Open VScode

(def vs-code-handler (cmd)
  "Initialize VS code."
  (run cmd "code" "gh/krei-systems.github.io"))
