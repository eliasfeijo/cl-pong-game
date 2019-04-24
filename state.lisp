;;;; state.lisp
(in-package :com.eliasfeijo.pong)

(defparameter *player1-score* 0)
(defparameter *player2-score* 0)

(defclass game-state () ())

(defmethod act ((this game-state))
  (declare (ignore this)))



;;; Game

(defclass game (game-state)
  ((player1 :initform (make-instance 'player))
   (player2 :initform (make-instance 'player))
   (ball :initform (make-instance 'ball))
   (last-updated :initform (real-time-seconds))))

(defmethod initialize-instance :after ((this game) &key)
  (with-slots (player1 player2 ball) this
    (setf
     *player1-score* 0
     *player2-score* 0
     (position-of player1) (vec2 (+ (x (size-of player1)) 20) (- (/ *canvas-height* 2) (/ (y (size-of player1)) 2)))
     (position-of player2) (vec2 (- *canvas-width* (+ (x (size-of player2)) 40))  (- (/ *canvas-height* 2) (/ (y (size-of player2)) 2)))
     (position-of ball) (vec2 (- (/ *canvas-width* 2) 5) (- (/ *canvas-height* 2) 5)))))

(defmethod press-key ((this game) key)
  (with-slots (player1 player2) this
    (cond
      ((eql key :w)
       (setf (moving-up-p player1) t))
      ((eql key :s)
       (setf (moving-down-p player1) t))
      ((eql key :up)
       (setf (moving-up-p player2) t))
      ((eql key :down)
       (setf (moving-down-p player2) t)))))

(defmethod release-key ((this game) key)
  (with-slots (player1 player2) this
    (cond
      ((eql key :w)
       (setf (moving-up-p player1) nil))
      ((eql key :s)
       (setf (moving-down-p player1) nil))
      ((eql key :up)
       (setf (moving-up-p player2) nil))
      ((eql key :down)
       (setf (moving-down-p player2) nil)))))

(defmethod render ((this game))
  (with-slots (player1 player2 ball) this
    (render player1)
    (render player2)
    (render ball))
  (draw-text (write-to-string *player1-score*) (vec2 20 580))
  (draw-text (write-to-string *player2-score*) (vec2 780 580)))

(defmethod act ((this game))
  (with-slots (player1 player2 ball last-updated) this
    (let* ((current-time (real-time-seconds))
	   (delta-time (- current-time last-updated)))
      (update-player player1 delta-time)
      (update-player player2 delta-time)
      (update-ball ball delta-time player1 player2)
      (setf last-updated current-time))))
