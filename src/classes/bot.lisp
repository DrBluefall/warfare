(defpackage :warfare.classes.bot
  (:use :cl)
  (:export :<bot> :bot-token))
(in-package :warfare.classes.bot)

(defclass <bot> ()
  "A discord bot."
  ((token :initarg :token
	  :initform (error "A bot needs a token to authorize with Discord.")
	  :reader bot-token)))
