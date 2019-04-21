(asdf:defsystem "pong"
  :description "Pong game made with trivial:gamekit."
  :version "0.0.1"
  :author "Elias Feijó"
  :license "Public Domain"
  :depends-on (trivial-gamekit)
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "pong")))
