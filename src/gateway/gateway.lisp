(defpackage :warfare.gateway
      (:use :cl :warfare.classes.gateway :warfare.classes.bot)
      (:import-from :warfare.http :send-request)
      (:export :set-websocket-info
               :connect-to-discord)
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
           (type <bot> bot))
  (let* ((wsinfo (bot-ws-info bot))
         (client (setf (bot-ws-connection wsinfo)
                       (wsd:make-client
                        (format nil "~a/?v=~d&encoding=json"
                                (bot-ws-url wsinfo)
                                warfare.constants:+discord-api-version+))))
         (event-queue (bot-ws-event-queue wsinfo)))
    (wsd:on :message client
            (lambda (ev)
              (log:debug "Event recieved!~%")
              (let* ((parsed (shasht:read-json ev))
                     (event (deserialize-gateway-event parsed)))
                (log:debug "Event: ~s~%json: ~s~%" event parsed)
                ;; Did a dumb thing here:
                ;;
                ;; When I was originally planning this library, I had gotten
                ;; annoyed that websocket-driver's read operations were handled
                ;; on a separate thread, and likely would've conflicted with my
                ;; desire to use cl-async for something like heartbeating.
                ;;
                ;; Then I completely forgot about that.
                ;;
                ;; I wrote much of the initial event processing code here, where
                ;; this comment is.
                ;;
                ;; Apparently, my initial idea should have been the one I went
                ;; for.
                ;;
                ;; For some godforsaken reason, cl-async's event loop isn't
                ;; available in websocket-driver's thread. I can only surmise
                ;; that something about cl-async's event loop is thread-local.
                (queues:qpush event-queue event))))
    (wsd:on :close client
            (lambda (&key code reason)
              (log:warn "Connection closed (~d): ~a" code reason)
              (setf (bot-ws-close-code wsinfo) code)))
    (wsd:start-connection client :verify nil)))
