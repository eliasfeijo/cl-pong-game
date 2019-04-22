;;;; pong.lisp
(in-package :com.eliasfeijo.pong)



(defvar *canvas-width* 800)
(defvar *canvas-height* 600)

(defgame pong ()
  ((player1 :initform (make-instance 'player :position (vec2 240 20)))
   (player2 :initform (make-instance 'player :position (vec2 240 560)))
   (last-updated :initform 0))
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "Pong"))

(defmethod post-initialize :after ((app pong))
  (with-slots (player1 player2) app
    ;;; Player 1
    (bind-button :left :pressed (lambda () (setf (moving-left-p player1) t)))
    (bind-button :left :released (lambda () (setf (moving-left-p player1) nil)))
    (bind-button :right :pressed (lambda () (setf (moving-right-p player1) t)))
    (bind-button :right :released (lambda () (setf (moving-right-p player1) nil)))
    ;;; Player 2
    (bind-button :a :pressed (lambda () (setf (moving-left-p player2) t)))
    (bind-button :a :released (lambda () (setf (moving-left-p player2) nil)))
    (bind-button :d :pressed (lambda () (setf (moving-right-p player2) t)))
    (bind-button :d :released (lambda () (setf (moving-right-p player2) nil)))))

(defvar *black* (vec4 0 0 0 1))

(defmethod draw ((app pong))
  (with-slots (player1 player2) app
    (draw-rect (position-of player1) (x (size-of player1)) (y (size-of player1)) :fill-paint (color-of player1))
    (draw-rect (position-of player2) (x (size-of player2)) (y (size-of player2)) :fill-paint (color-of player2))))

(defmethod act ((app pong))
  (with-slots (player1 player2 last-updated) app
    (let* ((current-time (real-time-seconds))
	   (delta-time (- current-time last-updated)))
      (update-player player1 delta-time)
      (update-player player2 delta-time)
      (setf last-updated current-time))))

(defun move (player direction delta-time)
  (let ((real-speed (* (speed-of player) delta-time)))
    (case direction
      (left
       (setf (x (position-of player)) (- (x (position-of player)) real-speed)))
      (right
       (setf (x (position-of player)) (+ (x (position-of player)) real-speed))))))

(defun update-player (player delta-time)
  (cond
    ((moving-left-p player) (move player 'left delta-time))
    ((moving-right-p player) (move player 'right delta-time))))
