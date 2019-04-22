(defpackage :com.eliasfeijo.pong
  (:use :common-lisp :alexandria :trivial-gamekit)
  (:shadowing-import-from :trivial-gamekit
			  :lerp)
  (:export play-game))
  
