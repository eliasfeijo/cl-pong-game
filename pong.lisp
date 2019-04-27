;;;; pong.lisp
(in-package :com.eliasfeijo.pong)

(register-resource-package :com.eliasfeijo.pong (asdf:system-relative-pathname :pong "assets/"))



(defgame pong ()
  ((game-state))
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "Pong")
  (:prepare-resources nil))

(defmethod post-initialize :after ((app pong))
  (with-slots (game-state) app
    (setf game-state (make-instance 'resource-preparation))
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
      (%bind-button :enter)
      (%bind-button :escape)))
  (prepare-resources 'player-red 'player-green 'player-blue))

(defmethod notice-resources ((this pong) &rest resource-names)
  (declare (ignore resource-names))
  (with-slots (game-state) this
    (labels ((game-over (winner)
	       (setf
		(game-over-p game-state) t
		(winner-of game-state) winner))
	     (start-game (&key player1-color player2-color)
	       (setf game-state (make-instance 'game :game-over #'game-over :color-selection-callback #'(lambda () (setf game-state (make-instance 'color-selection :start #'start-game)))))
	       (setf (color-of (player1-of game-state)) player1-color)
	       (setf (color-of (player2-of game-state)) player2-color)))
      (setf game-state (make-instance 'color-selection :start #'start-game)))))



(defmethod draw ((this pong))
  (with-slots (game-state) this
    (render game-state)))
      

(defmethod act ((this pong))
  (with-slots (game-state) this
    (act game-state)))

(defun play-game ()
  (start 'pong))
