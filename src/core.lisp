(uiop:define-package #:ems/src/core
  (:use #:cl))

(in-package #:ems/src/core)


;;; Special config

(defparameter *config*
    (list
     :name "ems"
     :description "CLI tool for managing Lisp nix flake"
     :version "1.0.0"
     :usage "[command] [options]"
     :dir (merge-pathnames #P"myflake/" (user-homedir-pathname))
     :time 4.5))


;;; Utilities

(defun get-config (key)
  "Get information from the *config*."
  (getf *config* key))

(defun log-msg (cmd fmt &rest args)
  "Log message if verbose mode is enabled."
  (when (clingon:getopt cmd :verbose)
    (apply #'format t fmt args)))

(defun run-cmd (cmd command &rest args)
  "Run a command with logging."
  (log-msg cmd "Running command: ~A ~{~A ~}~%" command args)
  (uiop:run-program (cons command args)
                    :input :interactive
                    :output :interactive
                    :error-output :interactive))

(defun run! (cmd command &rest args)
  "Safely execute commands in myflake directory with logging."
  (let ((dir (namestring (get-config :dir)))) 
    (log-msg cmd "Changing to directory: ~A~%" dir)
    (uiop:chdir dir)
    (apply #'run-cmd cmd command args)))

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
