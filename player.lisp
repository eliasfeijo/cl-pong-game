;;;; player.lisp
(in-package :com.eliasfeijo.pong)

(defclass player (positionable renderable actor)
  ((color
    :initarg :color
    :accessor color-of
    :initform nil)
   (size
    :initarg :size
    :accessor size-of
    :initform (vec2 20 100))
   (speed :initform 500 :accessor speed-of)
   (moving-up-p :initform nil :accessor moving-up-p)
   (moving-down-p :initform nil :accessor moving-down-p)
   (time-last-skill :initform (real-time-seconds) :accessor time-last-skill)))

(defun center-of (player)
  (let ((position (position-of player))
	(size (size-of player)))
    (vec2
     (+ (x position) (/ (x size) 2))
     (+ (y position) (/ (y size) 2)))))

(defun move (player direction delta-time)
  (let ((real-speed (* (speed-of player) delta-time)))
    (case direction
      (down
       (if (> (+ (y (position-of player)) (y (size-of player))) 0)
	   (setf (y (position-of player)) (- (y (position-of player)) real-speed))))
      (up
       (if (< (y (position-of player)) *canvas-height*)
	   (setf (y (position-of player)) (+ (y (position-of player)) real-speed)))))))

(defun update-player (player delta-time)
  (update-effects player)
  (cond
    ((moving-down-p player) (move player 'down delta-time))
    ((moving-up-p player) (move player 'up delta-time))))

(defmethod render ((this player))
  (draw-rect (position-of this) (x (size-of this)) (y (size-of this)) :fill-paint (value-of (color-of this))))


