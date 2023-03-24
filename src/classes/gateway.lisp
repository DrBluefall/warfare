(defpackage :warfare.classes.gateway
  (:use :cl)
  (:export
   :<gateway-event>
   :<event-heartbeat>
   :<event-heartbeat-ack>
   :<event-identify>
   :<event-hello>
   :hello-heartbeat-interval
   :deserialize-gateway-event
   :serialize-gateway-event))
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
                       :initarg :heartbeat-interval
                       :documentation "The delay to wait between sending heartbeat events.")))

(defclass <event-heartbeat> (<gateway-event>)
  ((seq-number :initarg :seq-number
               :accessor heartbeat-sequence-number
               :documentation "Sent to Discord in order to maintain a gateway connection. If Discord sends this event to us, immediately respond with our own heartbeat.")))

(defclass <event-heartbeat-ack> (<gateway-event>)
  ()
  (:documentation "A heartbeat acknowledgement from Discord. If this event isn't recieved between heartbeats, close the gateway & reconnect."))

(defclass <event-identify> (<gateway-event>)
  ((token :type string
          :initarg :token
          :initform (error "A token must be set to IDENTIFY with Discord.")
          :accessor identify-token
          :documentation "A bot's authentication token.")
   (properties :type list
               :initform `(("os"      . ,(format nil "~a ~a" (software-type) (software-version)))
                           ("browser" . "warfare")
                           ("device"  . "warfare"))
               :accessor identify-properties
               :documentation "Properties of the connecting client.")
   (compress :type boolean
             :initform nil
             :reader identify-compress
             :documentation "Whether the library supports packet compression or not.")
   (large-threshold :type (integer 50 250)
                    :initarg :large-threshold
                    :initform 50
                    :accessor identify-large-threshold
                    :documentation "The cap at which Discord will stop sending offline members in large guilds.")
   (shard :type (cons unsigned-byte unsigned-byte)
          :initarg :shard-info
          :accessor identify-shard-info
          :initform nil
          :documentation "Used for sharding. See <https://discord.com/developers/docs/topics/gateway#sharding>.")
   (intents :type integer
            :initarg :intent-int
            :accessor identify-intent-int
            :initform 0
            :documentation "Intents to request from the Gateway.")))

(defgeneric serialize-gateway-event (ev)
  (:documentation "Serialize a gateway event into JSON.")
  (:method ((ev <event-heartbeat>))
    (shasht:write-json* `(("op" . 1) ("d" . ,(heartbeat-sequence-number ev)))
                        :stream nil
                        :alist-as-object t
                        :false-values '(nil :null)))
  (:method ((ev <event-identify>))
    (let ((alist nil))
      (push '("op" . 2) alist) ; Opcode
      (let ((data-alist nil))
        (push `("token" . ,(identify-token ev)) data-alist)
        (push `("properties" . ,(identify-properties ev)) data-alist)
        (push `("compress" . ,(identify-compress ev)) data-alist)
        (push `("large_threshold" . ,(identify-large-threshold ev)) data-alist)
        (unless (null (identify-shard-info ev))
          (push `("shard" . ,(identify-shard-info ev)) data-alist))
        (push `("intents" . ,(identify-intent-int ev)) data-alist)
        (push `("d" . ,data-alist) alist))
      (shasht:write-json* alist
                          :alist-as-object t
                          :false-values '(nil :null)
                          :stream nil))))

(defun deserialize-gateway-event (ev)
  "Parse a gateway event into a proper class."
  (declare (optimize (speed 3))
           (type hash-table ev))
  (log:trace "Parsing event...")
  ;; Get the opcode & data. That'll inform the rest of the process.
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
      (log:trace "Event parsed with " opcode)
      (alexandria:switch (opcode :test #'=)
        ;; Used to maintain a gateway connection.
        ;; We may recieve this event from Discord, so we have to include it here.
        (1  (make-event '<event-heartbeat> :seq-number data))
        ;; Initial event sent when connecting.
        (10 (make-event '<event-hello> :heartbeat-interval (gethash "heartbeat_interval" data)))
        ;; Recognition of a heartbeat.
        (11 (make-event '<event-heartbeat-ack>))
        (t (make-event '<gateway-event>))))))
