;;;; state.lisp
(in-package :com.eliasfeijo.pong)

(defparameter *player1-score* 0)
(defparameter *player2-score* 0)

(defclass game-state () ())

(defmethod act ((this game-state))
  (declare (ignore this)))



;;; Color Selection

(defclass color-selection (game-state)
  ((p1-cursor :initform 0)
   (p2-cursor :initform 0)
   (p1-confirmed-p :initform nil)
   (p2-confirmed-p :initform nil)
   (start-callback :initarg :start)))

(defmethod press-key ((this color-selection) key)
  (with-slots (p1-cursor p2-cursor p1-confirmed-p p2-confirmed-p) this
    (cond
      ;;; Player 1
      ((eql key :w)
       (cond
	 ((and
	   (> p1-cursor 0)
	   (null p1-confirmed-p))
	  (setf p1-cursor (decf p1-cursor)))))
      ((eql key :s)
       (cond
	 ((and
	   (< p1-cursor (- (length *list-color*) 1))
	   (null p1-confirmed-p))
	  (setf p1-cursor (incf p1-cursor)))))
      ((eql key :space)
       (setf p1-confirmed-p t))
      ;;; Player 2
      ((eql key :up)
       (cond
	 ((and
	   (> p2-cursor 0)
	   (null p2-confirmed-p))
	  (setf p2-cursor (decf p2-cursor)))))
      ((eql key :down)
       (cond
	 ((and
	   (< p2-cursor (- (length *list-color*) 1))
	   (null p2-confirmed-p))
	  (setf p2-cursor (incf p2-cursor)))))
      ((eql key :enter)
       (setf p2-confirmed-p t)))))



(defmethod render ((this color-selection))
  (with-slots (p1-cursor p2-cursor p1-confirmed-p p2-confirmed-p) this
    (draw-rect (vec2 (/ *canvas-width* 2) 0) 1 (- *canvas-height* 50) :fill-paint *black*)
    (draw-text "Select your color" (vec2 (- (/ *canvas-width* 2) 50) (- *canvas-height* 30)))
    ;;; Player 1
    (draw-text "Player 1 (W and S):" (vec2 50 500))
    (loop
       for item in *list-color*
       for i from 1 to (length *list-color*)
       for text = (concat-cursor p1-cursor (- i 1) item)
       then (concat-cursor p1-cursor (- i 1) item)
       do (draw-text text (vec2 50 (- 480 (* 15 i)))))
    (draw-text (description-of (elt *list-color* p1-cursor)) (vec2 50 400) :fill-color (value-of (elt *list-color* p1-cursor)))
    (draw-text "Press space to confirm." (vec2 50 350))
    (if p1-confirmed-p (draw-text "Confirmed." (vec2 50 330) :fill-color (vec4 0 0.5 0 1)))
    ;;; Player 2
    (draw-text "Player 2 (Up and Down):" (vec2 450 500))
    (loop
       for item in *list-color*
       for i from 1 to (length *list-color*)
       for text = (concat-cursor p2-cursor (- i 1) item)
       then (concat-cursor p2-cursor (- i 1) item)
       do (draw-text text (vec2 450 (- 480 (* 15 i)))))
    (draw-text (description-of (elt *list-color* p2-cursor)) (vec2 450 400) :fill-color (value-of (elt *list-color* p2-cursor)))
    (draw-text "Press enter to confirm." (vec2 450 350))
    (if p2-confirmed-p (draw-text "Confirmed." (vec2 450 330) :fill-color (vec4 0 0.5 0 1)))))

(defun concat-cursor (cursor idx item)
  (if (eql cursor idx)
      (concatenate 'string "> " (name-of item))
      (name-of item)))

(defmethod act ((this color-selection))
  (with-slots (p1-confirmed-p p2-confirmed-p p1-cursor p2-cursor start-callback) this
    (if (and p1-confirmed-p p2-confirmed-p)
	(let ((p1-color (elt *list-color* p1-cursor))
	      (p2-color (elt *list-color* p2-cursor)))
	  (funcall start-callback :player1-color p1-color :player2-color p2-color)))))



;;; Game

(defclass game (game-state)
  ((player1 :initform (make-instance 'player) :accessor player1-of)
   (player2 :initform (make-instance 'player) :accessor player2-of)
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
