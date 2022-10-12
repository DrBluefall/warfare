(defpackage :warfare.http
  (:documentation "Tools for interacting with the Discord HTTP API.")
  (:use :cl)
  (:import-from :drakma :http-request)
  (:import-from :warfare.constants :+base-url+ :+discord-api-version+)
  (:import-from :alexandria :define-constant)
  (:import-from :warfare.classes.bot :<bot> :bot-token)
  (:import-from :warfare.conditions :warfare-error-http)
  (:export :send-request))
(in-package :warfare.http)

(define-constant +user-agent+ "DiscordBot (http://github.com/DrBluefall/warfare, 0.0.1)"
  :documentation "The user-agent header used to authenticate with the Discord API."
  :test #'string=)

(defun discord-api-url ()
  "Build the url for interacting with the API."
  (format nil "~a/v~d" +base-url+ +discord-api-version+))

(defun send-request (rq-type endpoint bot
         &aux
           (url (format nil "~a/~a" (discord-api-url) endpoint)))
  "Make an HTTP request to the Discord API.

`rq-type` is the type of request to fire.
`endpoint` is the endpoint to target.
`bot` is an instance of `warfare.classes.bot:<bot>`."

  (declare (type (or <bot> nil) bot)
     (type string endpoint)
     (type keyword rq-type)
     (optimize (safety 3) (speed 0) (debug 3)))
  (multiple-value-bind (body status headers uri stream must-close reason-phrase)
      (drakma:http-request url
         :user-agent (if bot +user-agent+ :drakma)
         :method rq-type
         :additional-headers (if bot
                `(("Authorization" . ,(format nil "Bot ~a" (bot-token bot))))))
    (declare (ignore headers stream must-close))
    (flet ((val-in-range (val min max)
       (and (>= val min) (<= val max))))
      ;; Signal an error on 4XX or 5XX status codes.
      (if (or (val-in-range status 400 499)
        (val-in-range status 500 599))
    (error 'warfare-error-http
     :what (format nil "Failed to send request to Discord: ~d ~a" status reason-phrase)
     :status status
     :uri uri
     :body body)))
    (let* ((json-str (flexi-streams:octets-to-string body))
     (json (shasht:read-json json-str)))
      json)))
