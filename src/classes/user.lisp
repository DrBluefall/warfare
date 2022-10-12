(defpackage :warfare.classes.user
  (:use :cl))
(in-package :warfare.classes.user)

(defclass <user> ()
  ((username :initarg :name
       :reader username)
   (discriminator :initarg :discriminator
      :reader discriminator)
   (id :initarg :id
       :reader snowflake)
   (accent-color :initarg :accent-color
     :reader accent-color)
   (avatar :initarg :avatar
     :reader avatar)
   (avatar-decoration :initarg :avatar-decoration
          :reader avatar-decoration)
   (banner :initarg :banner
     :reader banner)
   (banner-color :initarg :banner-color
     :reader banner-color)
   (public-flags :initarg :public-flags
     :reader public-flags)
   (bot :initarg :is-bot
  :reader is-bot)))
