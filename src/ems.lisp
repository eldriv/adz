(defpackage :ems
  (:use :cl)
  (:import-from :clingon)
  (:export :main))

(in-package :ems)


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
                    :output :interactive
                    :error-output :interactive))

(defun run! (cmd command &rest args)
  "Safely execute commands in myflake directory with logging."
  (let ((dir (namestring (get-config :dir)))) 
    (log-msg cmd "Changing to directory: ~A~%" dir)
    (uiop:chdir dir)
    (apply #'run-cmd cmd command args)))


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

(defun version-handler (cmd)
  "Check SBCL version."
  (run! cmd "nix" "develop" ".#lisp" "-c" "sbcl" "--version"))

(defmacro define-flake-command (name alias description handler)
  "Define a flake command with aliases prior to its handler."
  (let ((maker-name (intern (format nil "MAKE-~A-COMMAND" name))))
    `(def- ,maker-name ()
       (clingon:make-command
        :name ,name
        :aliases (list ,alias)
        :description ,description
        :handler ,handler))))

(define-flake-command "run" "r" "Run the Emacs shell" #'run-handler)
(define-flake-command "update" "u" "Update the Lisp nix flake" #'update-handler)
(define-flake-command "show" "s" "Show output attribute of the Lisp flake" #'show-handler)
(define-flake-command "sbcl-version" "sv" "Check SBCL's version" #'version-handler)


;;; top-level

(defmacro define-option (type short-name long-name description &key key)
  "Define a CLI option with standard structure"
  `(clingon:make-option
    ,type
    :short-name ,short-name
    :long-name ,long-name
    :description ,description
    :key ,(or key (intern (string-upcase long-name) "KEYWORD"))))

(defun make-cli-options ()
  "Create CLI options"
  (list
   (define-option :counter #\v "verbose" "Enable verbose output" :key :verbose)
   (define-option :string #\d "debug" "Enable debug mode" :key :debug)))

(defun top-level/handler (cmd)
  (let ((args (clingon:command-arguments cmd)))
    (if args
        (format t "Unknown command: ~A~%" (first args))
        (progn
          (format t "Usage: ~A~%" (get-config :usage))
          (clingon:print-usage cmd t)))))

(defun make-top-level-command ()
  "Top-level commands"
  (clingon:make-command
   :name (get-config :name)
   :description (get-config :description)
   :version (get-config :version)
   :usage (get-config :usage)
   :authors '("Eldriv")
   :options (make-cli-options)
   :handler #'top-level/handler
   :sub-commands (list
                  (make-run-command)
                  (make-update-command)
                  (make-show-command)
                  (make-sbcl-version-command))))

(defun main ()
  "Main entry point for the application"
  (let ((app (make-top-level-command)))
    (clingon:run app)))
