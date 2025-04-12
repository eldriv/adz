(uiop:define-package #:adz/src/core
  (:use #:cl
        #:marie))

(in-package #:adz/src/core)


;;; Special config

(defp *config*
    (list
     :name "adz"
     :description "A thin wrapper for interacting with my Lisp environment and others"
     :version "1.0.0"
     :usage "[main-command] [sub-command]"
     :author (list "eldriv <eldriv@krei.systems>")))

(defp *path*
    (list
     :flake #P"myflake/"
     :web #P"gh/krei-systems.github.io"
     :redmine-docker #P"gh/redmine-docker"
     :kb-docker #P"gh/wiki"))

(defp *hostname*
    (machine-instance))


;;; Utilities

(def path* ()
  "Get path then merge it to the homedir pathname."
  (loop :for x :on *path* :by #'cddr
        :when (∧ (keywordp (car x)) (cadr x))
          :append (list (car x) (merge-pathnames (cadr x) (user-homedir-pathname)))))

(def get-config (key)
  "Get information from the *config*."
  (getf *config* key))

(def get-paths (key)
  "Get path from the *path*"
  (getf (path*) key))

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

(def run* (cmd dat command &rest args)
  "Safely execute commands but with control of directory with logging."
  (let ((dir (namestring (get-paths dat))))
    (log-msg cmd "Changing to directory: ~A~%" dir)
    (uiop:chdir dir)
    (apply #'run-cmd cmd command args)))

(def run (cmd command &rest args)
  "Safely execute commands in current directory with logging."
  (let ((dir (namestring (uiop/os:getcwd))))
    (log-msg cmd "Changing to directory: ~A~%" dir)
    (uiop:chdir dir)
    (apply #'run-cmd cmd command args)))

(def top-level-handler (cmd)
  "Default handler for category commands - just prints help"
  (clingon:print-usage cmd t))

(def- intern-fn (name)
  "Intern a function name symbol from a command name symbol."
  (intern (format nil "MAKE-~A-COMMAND" (string-upcase (symbol-name name)))))

(defm define-main-commands (name (&optional alias)
                                 description
                                 handler
                                 sub-commands
                                 &optional usage)
  "Define a main command with sub commands."
  (let ((maker-name (intern-fn name)))
    `(def ,maker-name ()
       (clingon:make-command
        :name ,(string-downcase (symbol-name name))
        :aliases (list ,@(when alias `(,(string-downcase (symbol-name alias)))))
        :description ,description
        :handler (when (eq t ,handler)
                   #'top-level-handler)
        :sub-commands ,sub-commands
        :usage ,usage))))

(defm define-sub-command (name (&optional alias) description handler &optional usage)
  "Define a command with aliases prior to its handler."
  (let ((maker-name (intern-fn name)))
    `(def ,maker-name ()
       (clingon:make-command
        :name ,(string-downcase (symbol-name name))
        :aliases (list ,@(when alias `(,(string-downcase (symbol-name alias)))))
        :description ,description
        :handler ,handler
        :usage ,usage))))
