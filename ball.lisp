;;;; ball.lisp
(in-package :com.eliasfeijo.pong)

(defclass ball (positionable)
  ((color
    :initarg :color
    :accessor color-of
    :initform (vec4 0.0 0.0 0.0 1))
   (size
    :initarg :size
    :accessor size-of
    :initform (vec2 10 10))
   (speed :initform 300 :accessor speed-of)
   ;; The ball is always moving in one direction or another
   (moving-left-p :initform nil :accessor moving-left-p)
   (moving-down-p :initform nil :accessor moving-down-p)))

(defmethod render ((this ball))
  (draw-rect (position-of this) (x (size-of this)) (y (size-of this)) :fill-paint (color-of this)))

(defun score (player ball)
  (if (eql player 'player1)
      (incf *player1-score*)
      (incf *player2-score*))
  (setf
   (position-of ball) (vec2 (- (/ *canvas-width* 2) 5) (- (/ *canvas-height* 2) 5))
   (moving-down-p ball) (eql (random 2) 1)
   (moving-left-p ball) (eql (random 2) 1)))



(defun update-ball (ball delta-time player1 player2)
  (cond
    ;; Check for score
    ((<= (x (position-of ball)) 0) (score 'player2 ball))
    ((>= (x (position-of ball)) (+ *canvas-width* (x (size-of ball)))) (score 'player1 ball))
    ;;; Check collision with walls
    ((<= (y (position-of ball)) 0)
     (setf (moving-down-p ball) nil))
    ((>=
      (+ (y (position-of ball)) (y (size-of ball)))
      *canvas-height*)
     (setf (moving-down-p ball) t))
    ;;; Check collision with players
    ((colliding-with ball player1)
     (setf (moving-left-p ball) nil))
    ((colliding-with ball player2)
     (setf (moving-left-p ball) t)))
  (move-ball ball delta-time))



(defun colliding-with (ball player)
  (if
   (and
    (<=
     (x (position-of ball))
     (+ (x (position-of player)) (x (size-of player))))
    (>=
     (+ (x (position-of ball)) (x (size-of ball)))
     (x (position-of player))))
   ;; Ball x is inside
   (cond 
     ((and
       (<=
	(y (position-of ball))
	(+ (y (position-of player)) (y (size-of player))))
       (>=
	(+ (y (position-of ball)) (y (size-of ball)))
	(y (position-of player)))) t)) nil))



(defun move-ball (ball delta-time)
  (let ((real-speed (* (speed-of ball) delta-time)))
    (if
     (moving-down-p ball)
     (if
      (moving-left-p ball)
      (setf
       (x (position-of ball)) (- (x (position-of ball)) real-speed)
       (y (position-of ball)) (- (y (position-of ball)) real-speed))
      (setf
       (x (position-of ball)) (+ (x (position-of ball)) real-speed)
       (y (position-of ball)) (- (y (position-of ball)) real-speed)))
     (if
      (moving-left-p ball)
      (setf
       (x (position-of ball)) (- (x (position-of ball)) real-speed)
       (y (position-of ball)) (+ (y (position-of ball)) real-speed))
      (setf
       (x (position-of ball)) (+ (x (position-of ball)) real-speed)
       (y (position-of ball)) (+ (y (position-of ball)) real-speed))))))
