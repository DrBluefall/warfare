(defpackage :warfare.classes.gateway
  (:use :cl)
  (:export :<gateway-event> :deserialize-gateway-event))
(in-package :warfare.classes.gateway)

(defclass <gateway-event> ()
  ((opcode :accessor event-opcode
           :initarg :opcode
           :documentation "The opcode of the event.")
   (type :accessor event-type
         :initarg :type
         :documentation "The name of the event.")
   (data :accessor event-data
         :initarg :data
         :documentation "The JSON recieved from an event, encoded as a hash table.")
   (sequence-number :accessor event-seq-number
                    :initarg :seq-number
                    :documentation "Sequence number used for resuming sessions & heartbeating.")))

(defclass <event-hello> (<gateway-event>)
  ((heartbeat-interval :accessor hello-heartbeat-interval
                       :documentation "The delay to wait between sending heartbeat events.")))

(defclass <event-heartbeat> (<gateway-event>)
  ((seq-number :initarg :seq-number
               :accessor heartbeat-sequence-number
               :documentation "Sent to Discord in order to maintain a gateway connection. If Discord sends this event to us, immediately respond with our own heartbeat.")))

(defclass <event-heartbeat-ack> (<gateway-event>)
  ()
  (:documentation "A heartbeat acknowledgement from Discord. If this event isn't recieved between heartbeats, close the gateway & reconnect."))

(defgeneric serialize-gateway-event (ev)
  (:documentation "Serialize a gateway event into JSON.")
  (:method ((ev <event-heartbeat>))
    (shasht:write-json* `(("op" . 1) ("d" . ,(heartbeat-sequence-number ev))) :stream nil
                        :alist-as-object t
                        :false-values '(nil :null))
    ))

(defun deserialize-gateway-event (ev)
  "Parse a gateway event into a proper class."
  (declare (optimize (speed 3))
           (type hash-table ev))

  ;; STEP 1: Get the opcode & data. That'll inform the rest of the process.
  (let* ((opcode (gethash "op" ev))
         (data (gethash "d" ev))
         (seq-num (if (not (= opcode 0)) nil (gethash "s" ev)))
         (type (if (not (= opcode 0)) nil (gethash "t" ev))))
    (macrolet ((make-event (sym &rest args)
                 `(make-instance ,sym
                   :type type
                   :seq-number seq-num
                   :data data
                   :opcode opcode
                   ,@args)))
      (alexandria:switch (opcode :test #'=)
        ;; Used to maintain a gateway connection.
        ;; We may recieve this event from Discord, so we have to include it here.
        (1  (make-event '<event-heartbeat> :seq-number data))
        ;; Initial event sent when connecting.
        (10 (make-event '<event-hello> :heartbeat-interval (gethash "heartbeat_interval" ev)))
        ;; Recognition of a heartbeat.
        (11 (make-event '<event-heartbeat-ack>)))))
  )
