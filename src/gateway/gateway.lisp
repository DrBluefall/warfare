(defpackage :warfare.gateway
      (:use :cl)
      (:import-from :warfare.classes.bot :<bot> :<bot-websocket-info> :bot-ws-info)
      (:import-from :warfare.http :send-request)
      (:documentation "Interacting with the Discord Websocket."))
(in-package :warfare.gateway)

(defun set-websocket-info (bot)
  "Request the info needed for bot to interact with the gateway."
  (declare (optimize (speed 0) (safety 3) (debug 3))
     (type <bot> bot))
  (let* ((json (send-request :get "gateway/bot" bot))
   (sstart-limits (gethash "session_start_limit" json)))
    (setf (bot-ws-info bot)
    (make-instance '<bot-websocket-info>
       :url (gethash "url" json)
       :cap (gethash "total" sstart-limits)
       :left (gethash "remaining" sstart-limits)
       :reset-after (gethash "reset_after" sstart-limits)
       :max-concurrents (gethash "max_concurrency" sstart-limits)))))

(defun connect-to-discord (bot)
  "Open a gateway connection to Discord."
  (declare (optimize (speed 0) (safety 3) (debug 3))
     (type <bot> bot)
     (ignore bot)))
