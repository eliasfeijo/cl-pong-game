;;;; pong.lisp
(in-package :com.eliasfeijo.pong)



(defvar *canvas-width* 400)
(defvar *canvas-height* 600)

(defparameter *player1-score* 0)
(defparameter *player2-score* 0)

(defgame pong ()
  ((player1 :initform (make-instance 'player))
   (player2 :initform (make-instance 'player))
   (ball :initform (make-instance 'ball))
   (last-updated :initform (real-time-seconds)))
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "Pong"))

(defmethod post-initialize :after ((app pong))
  (with-slots (player1 player2 ball) app
    (setf
     *player1-score* 0
     *player2-score* 0
     (position-of player1) (vec2 (- (/ *canvas-width* 2) (/ (x (size-of player1)) 2)) (+ (y (size-of player1)) 20))
     (position-of player2) (vec2 (- (/ *canvas-width* 2) (/ (x (size-of player2)) 2)) (- *canvas-height* (+ (y (size-of player2)) 30)))
     (position-of ball) (vec2 (- (/ *canvas-width* 2) 5) (- (/ *canvas-height* 2) 5)))
    (bind-button :esc :pressed #'stop)
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
  (with-slots (player1 player2 ball) app
    (draw-rect (position-of player1) (x (size-of player1)) (y (size-of player1)) :fill-paint (color-of player1))
    (draw-rect (position-of player2) (x (size-of player2)) (y (size-of player2)) :fill-paint (color-of player2))
    (draw-rect (position-of ball) (x (size-of ball)) (y (size-of ball)) :fill-paint (color-of ball)))
  (draw-text (write-to-string *player1-score*) (vec2 20 20))
  (draw-text (write-to-string *player2-score*) (vec2 20 580)))

(defmethod act ((app pong))
  (with-slots (player1 player2 ball last-updated) app
    (let* ((current-time (real-time-seconds))
	   (delta-time (- current-time last-updated)))
      (update-player player1 delta-time)
      (update-player player2 delta-time)
      (update-ball ball delta-time player1 player2)
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
    ((<= (y (position-of ball)) 0) (score 'player2 ball))
    ((>= (y (position-of ball)) (+ *canvas-height* (/ (y (size-of ball)) 2))) (score 'player1 ball))
    ;;; Check collision with walls
    ((<= (x (position-of ball)) 0)
     (setf (moving-left-p ball) nil))
    ((>=
      (+ (x (position-of ball)) (x (size-of ball)))
      *canvas-width*)
     (setf (moving-left-p ball) t))
    ;;; Check collision with players
    ((colliding-with ball player1)
     (setf (moving-down-p ball) nil))
    ((colliding-with ball player2)
     (setf (moving-down-p ball) t)))
  (move-ball ball delta-time))



(defun colliding-with (ball player)
  (if
   (and
    (<=
     (y (position-of ball))
     (+ (y (position-of player)) (y (size-of player))))
    (>=
     (+ (y (position-of ball)) (y (size-of ball)))
     (y (position-of player))))
   ;; Ball y is inside
   (cond 
     ((and
       (<=
	(x (position-of ball))
	(+ (x (position-of player)) (x (size-of player))))
       (>=
	(+ (x (position-of ball)) (x (size-of ball)))
	(x (position-of player)))) t)) nil))



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

(defun play-game ()
  (start 'pong))
