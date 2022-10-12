(defpackage :warfare.conditions
  (:use :cl)
  (:export :warfare-error))
(in-package :warfare.conditions)

(define-condition warfare-error (error)
  ((what :initarg :what
   :initform "An unspecified error has occurred"
   :reader warfare-error-what))
  (:documentation "An error signaled from Warfare.")
  (:report (lambda (c stream)
       (format stream "WARFARE-ERROR: ~a" (warfare-error-what c)))))

(define-condition warfare-error-http (warfare-error)
  ((body :initarg :body
   :reader warfare-error-http-body
   :documentation "The body of the response returned")
   (status :initarg :status
     :reader warfare-error-http-status
     :documentation "The status code of the response.")
   (uri :initarg :uri
  :reader warfare-error-http-uri
  :documentation "The request the URI was sent to."))
  (:documentation "An error involving making an HTTP request to discord has occurred."))
