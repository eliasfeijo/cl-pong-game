(defpackage :com.eliasfeijo.pong
  (:use :common-lisp :alexandria :trivial-gamekit :array-utils)
  (:shadowing-import-from :trivial-gamekit
			  :lerp)
  (:export play-game))
  
