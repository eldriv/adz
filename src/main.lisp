(uiop:define-package #:adz/src/main
  (:use :cl
   :marie
   #:adz/src/core
   #:adz/src/run
   #:adz/src/commands)
  (:import-from :clingon)
  (:export :main))

(in-package #:adz/src/main)



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
   :sub-commands (top-level-commands)))

(def- main ()
  "Main entry point for the application"
  (let ((app (make-top-level-command)))
    (clingon:run app)))
