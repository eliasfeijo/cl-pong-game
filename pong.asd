(asdf:defsystem "pong"
  :description "Pong game made with trivial:gamekit"
  :version "0.0.1"
  :author "Elias Feijó"
  :license "Public Domain"
  :depends-on (alexandria bodge-utilities trivial-gamekit)
  :serial t
  :components ((:file "packages")
	       (:file "util")
	       (:file "player")
	       (:file "ball")
	       (:file "skill")
	       (:file "state")
	       (:file "pong")))
