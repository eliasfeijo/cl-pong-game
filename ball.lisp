;;;; ball.lisp
(in-package :com.eliasfeijo.pong)

(defparameter +max-score+ 5)

(defclass ball (positionable renderable actor)
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
   (moving-down-p :initform nil :accessor moving-down-p)
   (effects :initform (make-array 5 :fill-pointer 0) :accessor effects-of)))

(defmethod render ((this ball))
  (draw-rect (position-of this) (x (size-of this)) (y (size-of this)) :fill-paint (color-of this)))

(defun score (player ball game-over-callback)
  (if (eql player 'player1)
      (incf *player1-score*)
      (incf *player2-score*))
  (cond
    ((>= *player1-score* +max-score+)
     (funcall game-over-callback 'player1))
    ((>= *player2-score* +max-score+)
     (funcall game-over-callback 'player2)))
  (setf
   (position-of ball) (vec2 (- (/ *canvas-width* 2) 5) (- (/ *canvas-height* 2) 5))
   (moving-down-p ball) (eql (random 2) 1)
   (moving-left-p ball) (eql (random 2) 1))
  (loop
     for i from 0
     for effect across (effects-of ball)
     do
       (stop-effect effect)
       (vector-pop-position* (effects-of ball) i)))



(defun update-ball (ball delta-time player1 player2 game-over-callback)
  (update-effects ball)
  (cond
    ;; Check for score
    ((<= (x (position-of ball)) 0) (score 'player2 ball game-over-callback))
    ((>= (x (position-of ball)) (+ *canvas-width* (x (size-of ball)))) (score 'player1 ball game-over-callback))
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
