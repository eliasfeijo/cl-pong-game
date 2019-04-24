;;;; player.lisp
(in-package :com.eliasfeijo.pong)

(defclass player (positionable renderable)
  ((color
    :initarg :color
    :accessor color-of
    :initform (vec4 0.0 0.0 0.0 1))
   (size
    :initarg :size
    :accessor size-of
    :initform (vec2 20 100))
   (speed :initform 500 :accessor speed-of)
   (moving-up-p :initform nil :accessor moving-up-p)
   (moving-down-p :initform nil :accessor moving-down-p)))

(defun move (player direction delta-time)
  (let ((real-speed (* (speed-of player) delta-time)))
    (case direction
      (down
       (setf (y (position-of player)) (- (y (position-of player)) real-speed)))
      (up
       (setf (y (position-of player)) (+ (y (position-of player)) real-speed))))))

(defun update-player (player delta-time)
  (cond
    ((moving-down-p player) (move player 'down delta-time))
    ((moving-up-p player) (move player 'up delta-time))))

(defmethod render ((this player))
  (draw-rect (position-of this) (x (size-of this)) (y (size-of this)) :fill-paint (color-of this)))
