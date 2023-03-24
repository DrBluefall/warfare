(defpackage :warfare
  (:use :cl :warfare.constants :warfare.gateway :warfare.classes.gateway :warfare.classes.bot)
  (:export :begin-event-loop))
(in-package :warfare)

(define-condition connection-broken (serious-condition)
  ()
  (:documentation "A warning signaling a broken websocket connection."))

(defun begin-event-loop (bot &optional (callback #'(lambda (_) #|no-op|#)))
  "Begin recieving events from Discord, using the token from `BOT'.

This function does a few things:

- It first waits for `WARFARE.CLASSES.GATEWAY:<EVENT-HELLO>' to be recieved.
  `WARFARE.CLASSES.GATEWAY:<EVENT-HELLO>' provides the `HELLO-HEARTBEAT-INTERVAL', which we need to...

- ...spin up a heartbeat thread, which sends continuous
  `WARFARE.CLASSES.GATEWAY:<EVENT-HEARTBEAT>'s over the gateway.

- After heartbeating is set up, it sends
  `WARFARE.CLASSES.GATEWAY:<EVENT-IDENTIFY>' over the gateway.

NOTE: `CALLBACK' should block for as little as possible. Ideally, you'd use it
to spin up another thread that does actual event processing."

  (declare (type <bot> bot))

  ;; FIRST: Set up the websocket info & connect to discord.
  (set-websocket-info bot)
  (connect-to-discord bot)

  ;; SECOND: Set up a constant LOOP, based on continuously calling `QUEUES:QPOP' continuously.

  (loop with wsinfo = (bot-ws-info bot)
        with event-queue = (bot-ws-event-queue wsinfo)
        with conn = (bot-ws-connection wsinfo)
        with waiting-for-heartbeat-ack = nil

        for event = (queues:qpop event-queue :empty)

        when (and (not (eq event :empty)) (not (eq (wsd:ready-state conn) :closed)))
          do (progn
               (typecase event
               (<event-hello>
                (bt:make-thread
                 #'(lambda (&rest args)
                     (loop
                       with interval = (hello-heartbeat-interval event)
                       for first-beat = (* interval
                                               (random 1.0d0 #|The required 'jitter' for the first heartbeat|#))
                                            then nil
                       when waiting-for-heartbeat-ack
                         ;; This connection is dead and/or "zombified".
                         ;; Signal a condition to close & attempt reconnect.
                         ;;
                         ;; TODO: Actually *implement* reconnection.
                         do (progn
                              (wsd:close-connection conn)
                              (error 'connection-broken))
                       until (eq (wsd:ready-state conn) :closed)
                       do (progn
                            (sleep (if (null first-beat) interval first-beat))
                            (log:trace "Sending heartbeat...")
                            (wsd:send-text
                             conn
                             (serialize-gateway-event
                              (make-instance 'warfare.classes.gateway:<event-heartbeat>
                                             :seq-number (bot-ws-sequence-number wsinfo))))
                            (setf waiting-for-heartbeat-ack t))))
                 :name "warfare heartbeat thread")
                ;; Now that the heartbeat thread is running, let's IDENTIFY!
                (log:trace "Sending IDENTIFY event...")
                (let ((identify (make-instance 'warfare.classes.gateway:<event-identify>
                                               :token (bot-token bot)
                                               :intent-int (bot-intents bot))))
                  (wsd:send-text conn (serialize-gateway-event identify))
                  (log:trace "IDENTIFY sent!")))
               (<event-heartbeat-ack>
                (setf waiting-for-heartbeat-ack nil)
                (log:trace "Heartbeat ack recieved!"))
               (t (log:warn "Recieved unknown event: ~S (event name: '~A')" event (warfare.classes.gateway::event-type event))))
               (funcall callback event))
        else do (bt:thread-yield))) ; DO NOT hold the thread if we don't have an
                                    ; event to process. We'll have to rely on
                                    ; good faith that users also yield so that
                                    ; the LOOP can continue processing, but in
                                    ; threaded implementations, yielding threads
                                    ; is a necessity. Reason No. INF that CL
                                    ; needs coroutines & continuations.
