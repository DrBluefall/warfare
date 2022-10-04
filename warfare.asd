(asdf:defsystem :warfare
  :description "A Discord API library for Common Lisp."
  :version "0.0.1"
  :author "Alexander \"Dr. Bluefall\" Bisono <sbisonol@gmail.com>"
  :license "LGPL-3.0-or-later"
  :depends-on (:flexi-streams
	       :drakma
	       :websocket-driver
	       :shasht
	       :alexandria)
  :components
  ((:module "src"
    :serial t
    :components ((:file "constants")
		 (:module "classes"
		  :serial t
		  :components ((:file "bot")))
		 (:module "http"
		  :serial t
		  :components ((:file "http")))
		 (:file "lib")))))
