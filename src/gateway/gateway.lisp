(defpackage :warfare.gateway
      (:use :cl :warfare.classes.gateway :warfare.classes.bot)
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
           (type <bot> bot))
  (let* ((wsinfo (bot-ws-info bot))
         (client (setf (bot-ws-connection wsinfo)
                       (wsd:make-client
                        (format nil "~a/?v=~d&encoding=json"
                                (bot-ws-url wsinfo)
                                warfare.constants:+discord-api-version+))))
         (event-queue (bot-ws-event-queue wsinfo))
         (heartbeat-event)
         (waiting-for-hb-ack nil))
    (wsd:on :message client
            (lambda (ev)
              (log:debug "Event recieved!~%")
              (let* ((parsed (shasht:read-json ev))
                     (event (deserialize-gateway-event parsed)))
                (log:debug "Event: ~s~%json: ~s~%" event parsed)
                (typecase event
                  ;; On <event-hello>, we set up an `as:delay'
                  ;; to fire the initial heartbeat, so that we
                  ;; can follow the specified
                  ;; "heartbeat_interval * jitter" that's
                  ;; required from initial connections to the
                  ;; gateway. From there, set up an
                  ;; `as:interval' that'll keep firing
                  ;; heartbeats so long as we're still connected
                  ;; to the gateway.
                  ;;
                  ;; How do we make sure it stops beating when
                  ;; we disconnect? Simple (hopefully), just
                  ;; assign the event to a variable in the
                  ;; outermost `let*', then use that in `wsd:on'
                  ;; with `:close'.
                  ;;
                  ;; *However*, we will have to check if
                  ;; `heartbeat-event' is a function or event,
                  ;; as they have to be handled differently (the
                  ;; former is `funcall'ed, the latter is
                  ;; `remove-event'..ed. YES.)
                  (<event-hello>
                   (setf heartbeat-event
                         (as:with-delay ((* (hello-heartbeat-interval event)
                                            (random 1.0d0 #| the aformentioned 'jitter' |# )))
                           (let ((heartbeat
                                   (make-instance '<event-heartbeat>
                                                  :seq-number (bot-ws-sequence-number wsinfo))))
                             (wsd:send client (serialize-gateway-event heartbeat))
                             (setf waiting-for-hb-ack t)
                             (setf heartbeat-event
                                   (as:with-interval ((hello-heartbeat-interval event))
                                     (let ((heartbeat
                                             (make-instance '<event-heartbeat>
                                                            :seq-number (bot-ws-sequence-number wsinfo))))
                                       (wsd:send client (serialize-gateway-event heartbeat))
                                       (setf waiting-for-hb-ack t)))))))
                   ;; Now that the heartbeat loop is set up, now comes the
                   ;; interesting part, sending an IDENTIFY payload.
                   ;;
                   ;; ref: https://discord.com/developers/docs/topics/gateway-events#identify
                   (let ((identify (make-instance '<event-identify>
                                                  :token (bot-token bot)
                                                  :intent-int (bot-intents bot))))))
                  (<ev-heartbeat-ack> (setf waiting-for-hb-ack nil))

                  ;; ...we don't know what this event is, so we're gonna log it.
                  (t (log:warn "Unknown event recieved: "
                               (event-opcode event)
                               (event-type event)
                               (event-data event)
                               (event-seq-number event)))))))
    (wsd:on :close client
            (lambda (&key code reason)
              (log:warn "Connection closed (~d): ~a" code reason)))
    (wsd:start-connection client)))
