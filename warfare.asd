(asdf:defsystem :warfare
  :description "A Discord API library for Common Lisp."
  :version "0.0.1"
  :author "Alexander \"Dr. Bluefall\" Bisono <sbisonol@gmail.com>"
  :license "LGPL-3.0-or-later"
  :depends-on (:flexi-streams
               :drakma
               :websocket-driver
               :shasht
               :alexandria
               :queues.simple-cqueue
               :cl-async
               :log4cl)
  :components
  ((:module "src"
    :serial t
    :components ((:file "constants")
         (:file "conditions")
         (:module "classes"
          :serial t
          :components ((:file "user")
                       (:file "bot")
                       (:file "gateway")))
         (:module "http"
          :serial t
          :components ((:file "http")))
         (:module "gateway"
          :serial t
          :components ((:file "gateway")))
                       (:file "lib")))))
