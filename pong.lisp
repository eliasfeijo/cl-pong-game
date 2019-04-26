;;;; pong.lisp
(in-package :com.eliasfeijo.pong)



(defgame pong ()
  ((game-state))
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "Pong"))

(defmethod post-initialize :after ((app pong))
  (with-slots (game-state) app
    (labels ((start-game ()
	       (setf game-state (make-instance 'game))))
      (setf game-state (make-instance 'color-selection :start #'start-game)))
	       
    (labels ((%bind-button (button)
	       (bind-button button :pressed
			    (lambda ()
			      (press-key game-state button)))
	       (bind-button button :released
			    (lambda ()
			      (release-key game-state button)))))
      (%bind-button :w)
      (%bind-button :s)
      (%bind-button :space)
      (%bind-button :up)
      (%bind-button :down)
      (%bind-button :enter))
    (bind-button :escape :pressed #'stop)))



(defmethod draw ((this pong))
  (with-slots (game-state) this
    (render game-state)))
      

(defmethod act ((this pong))
  (with-slots (game-state) this
    (act game-state)))

(defun play-game ()
  (start 'pong))
