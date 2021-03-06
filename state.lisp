;;;; state.lisp
(in-package :com.eliasfeijo.pong)

(defparameter *player1-score* 0)
(defparameter *player2-score* 0)

(defparameter *background-color* (vec4 1 1 0 0.7))

(define-font 'samurai-bob "fonts/CFSamuraiBob.ttf")
(define-font 'data "fonts/data-latin.ttf")

(defclass game-state () ())

(defmethod act ((this game-state))
  (declare (ignore this)))

;;; Resource Preparation

(defclass resource-preparation (game-state) ())

(defmethod render ((this resource-preparation))
  (draw-rect *canvas-origin* *canvas-width* *canvas-height* :fill-paint *black*)
  (draw-text "Loading..." (vec2 350 300) :fill-color *white*))

;;; Initial Screen

(defclass initial-screen (game-state)
  ((title-font :initform (make-font 'samurai-bob 128))
   (header-font :initform (make-font 'data 62))
   (subheader-font :initform (make-font 'data 48))
   (text-font :initform (make-font 'data 32))
   (text2-font :initform (make-font 'data 28))
   (p1-confirmed-p :initform nil)
   (p2-confirmed-p :initform nil)
   (color-selection-callback :initarg :color-selection)))

(defmethod press-key ((this initial-screen) key)
  (with-slots (p1-confirmed-p p2-confirmed-p) this
    (cond
      ((eql key :escape)
       (stop))
      ((eql key :space)
       (setf p1-confirmed-p t))
      ((eql key :enter)
       (setf p2-confirmed-p t)))))

(defmethod act ((this initial-screen))
  (with-slots (p1-confirmed-p p2-confirmed-p color-selection-callback) this
    (if (and p1-confirmed-p p2-confirmed-p)
	(funcall color-selection-callback))))

(defmethod render ((this initial-screen))
  (with-slots (title-font header-font subheader-font text-font text2-font p1-confirmed-p p2-confirmed-p) this
    (draw-rect *canvas-origin* *canvas-width* *canvas-height* :fill-paint *background-color*)
    (draw-text "Pong Fight" (vec2 160 500) :fill-color *black* :font title-font)
    (draw-text "by Elias Feijo" (vec2 165 450) :fill-color *black* :font text-font)
    ;; "ó" accent
    (draw-line (vec2 360 470) (vec2 363 474) *black* :thickness 2)
    ;; Box
    (draw-rect (vec2 150 80) 500 350 :stroke-paint *black*)
    (draw-text "Instructions" (vec2 230 370) :fill-color *black* :font header-font)
    (draw-rect (vec2 400 80) 1 250 :stroke-paint *black*)
    ;;; Player 1 instructions
    (draw-text "Player 1" (vec2 180 300) :fill-color *black* :font subheader-font)
    (draw-text "W - move up" (vec2 180 250) :fill-color *black* :font text2-font)
    (draw-text "S - move down" (vec2 180 200) :fill-color *black* :font text2-font)
    (draw-text "Space - shoot" (vec2 180 150) :fill-color *black* :font text2-font)
    ;;; Player 2 instructions
    (draw-text "Player 2" (vec2 430 300) :fill-color *black* :font subheader-font)
    (draw-text "Up - move up" (vec2 430 250) :fill-color *black* :font text2-font)
    (draw-text "Down - move down" (vec2 430 200) :fill-color *black* :font text2-font)
    (draw-text "Enter - shoot" (vec2 430 150) :fill-color *black* :font text2-font)
    (if p1-confirmed-p
	(draw-text "Confirmed" (vec2 200 100) :fill-color (vec4 0 0.6 0 1) :font text-font)
	(draw-text "Shoot to confirm" (vec2 160 100) :fill-color *black* :font text-font))
    (if p2-confirmed-p
	(draw-text "Confirmed" (vec2 450 100) :fill-color (vec4 0 0.6 0 1) :font text-font)
	(draw-text "Shoot to confirm" (vec2 410 100) :fill-color *black* :font text-font))))

;;; Color Selection

(defclass color-selection (game-state)
  ((p1-cursor :initform 0)
   (p2-cursor :initform 0)
   (p1-confirmed-p :initform nil)
   (p2-confirmed-p :initform nil)
   (start-callback :initarg :start)
   (subheader-font :initform (make-font 'data 48))
   (text-font :initform (make-font 'data 32))
   (text2-font :initform (make-font 'data 28))))

(defmethod press-key ((this color-selection) key)
  (with-slots (p1-cursor p2-cursor p1-confirmed-p p2-confirmed-p) this
    (cond
      ((eql key :escape)
       (stop))
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
  (with-slots (p1-cursor p2-cursor p1-confirmed-p p2-confirmed-p subheader-font text-font text2-font) this
    (draw-rect *canvas-origin* *canvas-width* *canvas-height* :fill-paint *background-color*)
    (draw-rect (vec2 (/ *canvas-width* 2) 0) 1 (- *canvas-height* 80) :fill-paint *black*)
    (draw-text "Select your color" (vec2 (- (/ *canvas-width* 2) 170) (- *canvas-height* 50)) :font subheader-font)
    ;;; Player 1
    (draw-text "Player 1 (W and S):" (vec2 50 500) :font text-font)
    (loop
       for item in *list-color*
       for i from 1 to (length *list-color*)
       for text = (concat-cursor p1-cursor (- i 1) item)
       then (concat-cursor p1-cursor (- i 1) item)
       do (draw-text text (vec2 50 (- 480 (* 35 i))) :font text2-font))
    (draw-text (description-of (elt *list-color* p1-cursor)) (vec2 50 320) :fill-color (value-of (elt *list-color* p1-cursor)) :font text2-font)
    (draw-text "Press space to confirm." (vec2 50 250) :font text-font)
    (if p1-confirmed-p (draw-text "Confirmed." (vec2 50 210) :fill-color (vec4 0 0.5 0 1) :font text-font))
    ;;; Player 2
    (draw-text "Player 2 (Up and Down):" (vec2 450 500) :font text-font)
    (loop
       for item in *list-color*
       for i from 1 to (length *list-color*)
       for text = (concat-cursor p2-cursor (- i 1) item)
       then (concat-cursor p2-cursor (- i 1) item)
       do (draw-text text (vec2 450 (- 480 (* 35 i))) :font text2-font))
    (draw-text (description-of (elt *list-color* p2-cursor)) (vec2 450 320) :fill-color (value-of (elt *list-color* p2-cursor)) :font text2-font)
    (draw-text "Press enter to confirm." (vec2 450 250) :font text-font)
    (if p2-confirmed-p (draw-text "Confirmed." (vec2 450 210) :fill-color (vec4 0 0.5 0 1) :font text-font))))

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
  ((player1 :initform (make-instance 'player) :reader player1-of)
   (player2 :initform (make-instance 'player :flipped-p t) :reader player2-of)
   (ball :initform (make-instance 'ball))
   (last-updated :initform (real-time-seconds))
   (skills :initform (make-array 10 :fill-pointer 0))
   (color-selection-callback :initarg :color-selection-callback)
   (game-over-callback :initarg :game-over)
   (game-over-p :initform nil :accessor game-over-p)
   (winner :initform nil :accessor winner-of)
   (score-font :initform (make-font 'data 48))
   (text-font :initform (make-font 'data 32))))

(defmethod initialize-instance :after ((this game) &key)
  (with-slots (player1 player2 ball) this
    (setf
     *player1-score* 0
     *player2-score* 0
     (position-of player1) (vec2 (+ (x (size-of player1)) 20) (- (/ *canvas-height* 2) (/ (y (size-of player1)) 2)))
     (position-of player2) (vec2 (- *canvas-width* (+ (x (size-of player2)) 40))  (- (/ *canvas-height* 2) (/ (y (size-of player2)) 2)))
     (position-of ball) (vec2 (- (/ *canvas-width* 2) 5) (- (/ *canvas-height* 2) 5)))))


(defmethod press-key ((this game) key)
  (with-slots (player1 player2 skills game-over-p color-selection-callback) this
    (if game-over-p
	(cond
	  ((or (eql key :space) (eql key :enter) (eql key :escape))
	   (funcall color-selection-callback)))
	(cond
	  ((eql key :escape)
	   (funcall color-selection-callback))
	  ((eql key :w)
	   (setf (moving-up-p player1) t))
	  ((eql key :s)
	   (setf (moving-down-p player1) t))
	  ((eql key :space)
	   (push-skill skills player1 'player2))
	  ((eql key :up)
	   (setf (moving-up-p player2) t))
	  ((eql key :down)
	   (setf (moving-down-p player2) t))
	  ((eql key :enter)
	   (push-skill skills player2 'player1))))))


(defun push-skill (skills player target)
  (cond
    ;;; Red skill
    ((string-equal (name-of (color-of player)) 'red)
     (if (> (- (real-time-seconds) (time-last-skill player)) +delay-red-skill+)
	 (progn
	   (vector-push
	    (make-instance 'red-skill
			   :position (center-of player)
			   :target target
			   :flipped-p (string-equal target 'player1)) skills)
	   (setf (time-last-skill player) (real-time-seconds)))))
    ;;; Green skill
    ((string-equal (name-of (color-of player)) 'green)
     (if (> (- (real-time-seconds) (time-last-skill player)) +delay-green-skill+)
	 (progn
	   (vector-push
	    (make-instance 'green-skill
			   :position (center-of player)
			   :target target
			   :flipped-p (string-equal target 'player1)) skills)
	   (setf (time-last-skill player) (real-time-seconds)))))
    ;;; Blue skill
    ((string-equal (name-of (color-of player)) 'blue)
     (if (> (- (real-time-seconds) (time-last-skill player)) +delay-blue-skill+)
	 (progn
	   (vector-push
	    (make-instance 'blue-skill
			   :position (center-of player)
			   :target target
			   :flipped-p (string-equal target 'player1)) skills)
	   (setf (time-last-skill player) (real-time-seconds)))))))

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
  (with-slots (player1 player2 ball skills game-over-p winner score-font text-font) this
    (draw-rect *canvas-origin* *canvas-width* *canvas-height* :fill-paint *background-color*)
    (render player1)
    (render player2)
    (render ball)
    (loop for skill across skills do (render skill))
    (draw-text (write-to-string *player1-score*) (vec2 300 550) :font score-font)
    (draw-text (write-to-string *player2-score*) (vec2 480 550) :font score-font)
    (if game-over-p
	(if (eql winner 'player1)
	    (draw-text "Player 1 wins." (vec2 300 400) :font text-font)
	    (draw-text "Player 2 wins." (vec2 300 400) :font text-font)))))


(defmethod act ((this game))
  (with-slots (player1 player2 ball last-updated skills game-over-p game-over-callback) this
    (unless game-over-p
      (let* ((current-time (real-time-seconds))
	     (delta-time (- current-time last-updated)))
	(update-player player1 delta-time)
	(update-player player2 delta-time)
	(update-ball ball delta-time player1 player2 game-over-callback)
	(remove-skills-outside-of-canvas skills)
	(remove-skills-colliding-with-player skills)
	(loop for skill across skills do (update-skill skill player1 player2 ball delta-time))
	(setf last-updated current-time)))))

(defun remove-skills-colliding-with-player (skills)
  (loop
     for i from 0
     for skill across skills
     do (if (collided-p skill)
	    (vector-pop-position* skills i))))

(defun remove-skills-outside-of-canvas (skills)
  (loop
     for i from 0
     for skill across skills
     do (unless (inside-canvas skill)
	  (vector-pop-position* skills i))))

(defun inside-canvas (skill)
  (and
    (>= (+ (x (position-of skill)) (x (size-of skill))) 0)
    (<= (x (position-of skill)) *canvas-width*)))
