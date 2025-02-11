(uiop:define-package #:ems/src/main
  (:use :cl
   :marie
   #:ems/src/core
   #:ems/src/commands)
  (:import-from :clingon)
  (:export :main))

(in-package #:ems/src/main)

;;; top-level

(defm define-option (type short-name long-name description &key key)
  "Define a CLI option with standard structure"
  `(clingon:make-option
    ,type
    :short-name ,short-name
    :long-name ,long-name
    :description ,description
    :key ,(or key (read-from-string (format nil ":~:@(~A~)" long-name)))))

(def- make-cli-options ()
  "Create CLI options"
  (list
   (define-option :counter #\v "verbose" "Enable verbose output" :key :verbose)
   (define-option :string #\d "debug" "Enable debug mode" :key :debug)))

(def- top-level-handler (cmd)
  "Checks if there are any extra arguments, if there's any and if it's an
  unknown command return first condition, Otherwise return the general usage instructions."
  (let ((args (clingon:command-arguments cmd)))
    (cond (args (format t "Unknown command: ~A~%" (first args)))
          (t (progn (format t "Usage: ~A~%" (get-config :usage))
                    (clingon:print-usage cmd t))))))

(def- make-top-level-command ()
  "Top-level commands"
  (clingon:make-command
   :name (get-config :name)
   :description (get-config :description)
   :version (get-config :version)
   :usage (get-config :usage)
   :authors '("Eldriv <michael.adrian.villareal@valmiz.com>")
   :options (make-cli-options)
   :handler #'top-level-handler
   :sub-commands (list
                  (make-run-command)
                  (make-update-command)
                  (make-show-command)
                  (make-sbcl-version-command)
                  (make-sbcl-command)
                  (make-shell-command))))

(def- main ()
  "Main entry point for the application"
  (let ((app (make-top-level-command)))
    (clingon:run app)))
