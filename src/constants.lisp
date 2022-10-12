(defpackage :warfare.constants
  (:documentation "Warfare constants.")
  (:use :cl)
  (:import-from :alexandria :define-constant))
(in-package :warfare.constants)

(define-constant +base-url+ "https://discord.com/api"
  :documentation "The base url for accessing the Discord API."
  :test #'string=)

(defparameter +discord-api-version+ 10
  "The version of the Discord API that Warfare is currently targeting.")

