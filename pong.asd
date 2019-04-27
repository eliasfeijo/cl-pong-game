(asdf:defsystem "pong"
  :description "Pong game made with trivial:gamekit"
  :version "0.0.1"
  :author "Elias Feijó"
  :license "Public Domain"
  :depends-on (alexandria bodge-utilities trivial-gamekit array-utils)
  :serial t
  :components ((:file "packages")
	       (:file "util")
	       (:file "player")
	       (:file "animation")
	       (:file "ball")
	       (:file "skill")
	       (:file "effect")
	       (:file "state")
	       (:file "pong")))
