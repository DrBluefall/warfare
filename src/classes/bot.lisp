(defpackage :warfare.classes.bot
  (:use :cl)
  (:export
   :<bot>
   :bot-token
   :bot-intents
   :<bot-websocket-info>
   :bot-ws-info
   :bot-ws-event-queue
   :bot-ws-connection
   :bot-ws-url
   :bot-ws-close-code))
(in-package :warfare.classes.bot)

(defclass <bot-websocket-info> ()
  ((conn :accessor bot-ws-connection
   :documentation "The current websocket connection to Discord.")
   (close-code :accessor bot-ws-close-code
               :initform nil
               :documentation "The close code for this websocket connection.")
   (url :accessor bot-ws-url
  :initarg :url
  :documentation "The url required to connect to the gateway.")
   (cap :accessor bot-ws-connection-cap
  :initarg :cap
  :documentation "The maximum number of times we can connect to the gateway.")
   (left :accessor bot-ws-connections-remaining
   :initarg :left
   :documentation "How many times we can connect to the gateway again.")
   (reset-after :accessor bot-ws-conncap-reset-after
    :initarg :reset-after
    :documentation "A timestamp representing how long until the the limit on gateway connections resets.")
   (max-concurrents :accessor bot-ws-max-concurrents
        :initarg :max-concurrents
        :documentation "The maximum number of IDENTIFY requests that can be sent every 5 seconds.")
   (queue :accessor bot-ws-event-queue
          :initform (queues:make-queue :simple-cqueue)
          :documentation "The queue where events recieved over the websocket are placed.")
   (seq-number :accessor bot-ws-sequence-number
               :initform nil
               :documentation "The last sequence number sent to us by Discord.")
   (heartbeat-thread :accessor bot-ws-heartbeat-thread
                     :initform nil
                     :documentation "The thread that sends heartbeats through this websocket connection.")))

(defclass <bot> ()
  ((token :initarg :token
    :initform (error "A bot needs a token to authorize with Discord.")
    :reader bot-token)
   (ws-info :type (or <bot-websocket-info> nil)
      :accessor bot-ws-info)
   (intents :accessor bot-intents
            :initform 0
            :documentation "A set of bitflags for describing what events should be recieved from Discord."))
  (:documentation "A discord bot."))

(defmethod (setf bot-intents) ((intent symbol) bot)
  (let ((intent-mask (alexandria:switch ((if (keywordp intent) intent
                                             (alexandria:make-keyword (string intent))))
                       (:guilds                        (ash 1 0))
                       (:guild-members                 (ash 1 1))
                       (:guild-bans                    (ash 1 2))
                       (:guild-emojis-and-stickers     (ash 1 3))
                       (:guild-integrations            (ash 1 4))
                       (:guild-webhooks                (ash 1 5))
                       (:guild-invites                 (ash 1 6))
                       (:guild-voice-states            (ash 1 7))
                       (:guild-presences               (ash 1 8))
                       (:guild-messages                (ash 1 9))
                       (:guild-message-reactions       (ash 1 10))
                       (:guild-message-typing          (ash 1 11))
                       (:direct-messages               (ash 1 12))
                       (:direct-message-reactions      (ash 1 13))
                       (:direct-message-typing         (ash 1 14))
                       (:message-content               (ash 1 15))
                       (:guild-scheduled-events        (ash 1 16))
                       (:auto-moderation-configuration (ash 1 20)))))
    (setf (slot-value bot 'intents) (logior (slot-value bot 'intents) intent-mask))))
