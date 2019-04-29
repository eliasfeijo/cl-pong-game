;;;; skill.lisp
(in-package :com.eliasfeijo.pong)

(defparameter +delay-red-skill+ 1.0)
(defparameter +delay-green-skill+ 2.0)
(defparameter +delay-blue-skill+ 2.5)

(define-image 'red-skill "images/skill-red.png")
(define-image 'green-skill "images/skill-green.png")
(define-image 'blue-skill "images/skill-blue.png")

(defparameter *anim-red-skill* (make-animation
				'((0 0 50 50 0)
				  (50 0 100 50 0.1)
				  (100 0 150 50 0.2)
				  (150 0 200 50 0.3)
				  (200 0 250 50 0.4)
				  (250 0 300 50 0.5)
				  (300 0 350 50 0.6)
				  (350 0 400 50 0.7)
				  (400 0 450 50 0.8)
				  (450 0 500 50 0.9))
				:looped-p t))
(defparameter *anim-green-skill* (make-animation
				  '((0 0 30 10 0)
				    (30 0 60 10 0.25)
				    (60 0 90 10 0.5))
				  :looped-p t))
(defparameter *anim-blue-skill* (make-animation
				 '((0 0 100 100 0)
				   (100 0 200 100 0.1)
				   (200 0 300 100 0.2)
				   (300 0 400 100 0.3)
				   (400 0 500 100 0.4)
				   (500 0 600 100 0.5)
				   (600 0 700 100 0.6)
				   (700 0 800 100 0.7)
				   (800 0 900 100 0.8)
				   (900 0 1000 100 0.9)
				   (1000 0 1100 100 1.0)
				   (1100 0 1200 100 1.1)
				   (1200 0 1300 100 1.2)
				   (1300 0 1400 100 1.3)
				   (1400 0 1500 100 1.4)
				   (1500 0 1600 100 1.5)
				   (1600 0 1700 100 1.6)
				   (1700 0 1800 100 1.7))
				 :looped-p t))


(defclass skill (positionable renderable)
  ((target :initarg :target :initform nil)
   (size :initarg :size :initform nil :accessor size-of)
   (speed :initarg :speed :initform nil :accessor speed-of)
   (fill-color :initarg :fill-color :initform nil :accessor fill-color-of)
   (collided-p :initform nil :reader collided-p)
   (flipped-p :initarg :flipped-p :initform nil)))

(defgeneric update-skill (skill player1 player2 ball delta-time))
(defgeneric move-skill (skill direction delta-time &key &allow-other-keys))

(defmethod render ((this skill))
  (with-slots (position size fill-color) this
    (draw-rect position (x size) (y size) :fill-paint fill-color)))

(defun center-of (skill)
  (let ((position (position-of skill))
	(size (size-of skill)))
    (vec2
     (+ (x position) (/ (x size) 2))
     (+ (y position) (/ (y size) 2)))))

;;; Red skill

(defclass red-skill (skill)
  ((size :initform (vec2 25 25))
   (speed :initform 700)
   (fill-color :initform (vec4 1 0 0 1))))

(defmethod initialize-instance :after ((this red-skill) &key)
  (with-slots (position size) this
    (setf position
	  (vec2
	   (x position)
	   (- (y position) (/ (y size) 2))))))

(defmethod render ((this red-skill))
  (with-slots (position size fill-color flipped-p) this
    (let* ((frame (get-frame *anim-red-skill* (real-time-seconds)))
	   (origin (keyframe-origin frame))
	   (end (keyframe-end frame)))
      (with-pushed-canvas ()
	(scale-canvas 0.5 0.5)
	(if flipped-p
	    (with-pushed-canvas ()
	      (scale-canvas -1 1)
	      (draw-image (vec2 (* (+ (- (x position)) (- (x size))) 2) (* (y position) 2)) 'red-skill
			  :origin origin
			  :width (- (x end) (x origin))
			  :height (- (y end) (y origin))))
	    (draw-image (vec2 (* (x position) 2) (* (y position) 2)) 'red-skill
			:origin origin
			:width (- (x end) (x origin))
			:height (- (y end) (y origin))))))))
  
(defmethod update-skill ((this red-skill) player1 player2 ball delta-time)
  (with-slots (target speed collided-p) this
    (if (eql target 'player1)
	(progn
	  (move-skill this 'left delta-time)
	  (if (colliding-with this player1)
	      (let ((burn (make-instance 'burning :target player1)))
		(setf collided-p t)
		(push-effect player1 burn))))
	(progn
	  (move-skill this 'right delta-time)
	  (if (colliding-with this player2)
	      (let ((burn (make-instance 'burning :target player2)))
		(setf collided-p t)
		(push-effect player2 burn)))))))

(defmethod move-skill ((this red-skill) direction delta-time &key)
  (with-slots (speed position) this
    (let ((real-speed (* speed delta-time)))
      (if (eql direction 'left)
	  (setf (x position) (- (x position) real-speed))
	  (setf (x position) (+ (x position) real-speed))))))

;;; Green skill

(defclass green-skill (skill)
  ((size :initform (vec2 30 10))
   (speed :initform 600)
   (speed-y :initform 300)
   (fill-color :initform (vec4 0 1 0 1))))

(defmethod initialize-instance :after ((this green-skill) &key)
  (with-slots (position size) this
    (setf position
	  (vec2
	   (x position)
	   (- (y position) (/ (y size) 2))))))

(defmethod render ((this green-skill))
  (with-slots (position size fill-color flipped-p) this
    (let* ((frame (get-frame *anim-green-skill* (real-time-seconds)))
	   (origin (keyframe-origin frame))
	   (end (keyframe-end frame)))
      (if flipped-p
	  (with-pushed-canvas ()
	    (scale-canvas -1 1)
	    (draw-image (vec2 (+ (- (x position)) (- (x size))) (y position)) 'green-skill
			:origin origin
			:width (- (x end) (x origin))
			:height (- (y end) (y origin))))
	  (draw-image position 'green-skill
		      :origin origin
		      :width (- (x end) (x origin))
		      :height (- (y end) (y origin)))))))

(defmethod update-skill ((this green-skill) player1 player2 ball delta-time)
  (with-slots (target speed collided-p) this
    (if (eql target 'player1)
	(progn
	  (move-skill this 'left delta-time :player1 player1 :player2 player2)
	  (if (colliding-with this player1)
	      (let ((slow (make-instance 'slow :target player1)))
		(setf collided-p t)
		(push-effect player1 slow))))
	(progn
	  (move-skill this 'right delta-time :player1 player1 :player2 player2)
	  (if (colliding-with this player2)
	      (let ((slow (make-instance 'slow :target player2)))
		(setf collided-p t)
		(push-effect player2 slow)))))))

(defmethod move-skill ((this green-skill) direction delta-time &key player1 player2)
  (with-slots (speed speed-y position) this
    (let ((real-speed (* speed delta-time))
	  (real-speed-y (* speed-y delta-time)))
      (if (eql direction 'left)
	  (let ((target (y (center-of player1))))
	    (setf (x position) (- (x position) real-speed))
	    (if (< (y position) (- target 50))
		(setf (y position) (+ (y position) real-speed-y))
		(if (> (y position) (+ target 50))
		    (setf (y position) (- (y position) real-speed-y)))))
	  (let ((target (y (center-of player2))))
	    (setf (x position) (+ (x position) real-speed))
	    (if (< (y position) (- target 50))
		(setf (y position) (+ (y position) real-speed-y))
		(if (> (y position) (+ target 50))
		    (setf (y position) (- (y position) real-speed-y)))))))))

;;; Blue skill

(defclass blue-skill (skill)
  ((size :initform (vec2 20 100))
   (speed :initform 400)
   (fill-color :initform (vec4 0 0 1 1))))

(defmethod initialize-instance :after ((this blue-skill) &key)
  (with-slots (position size) this
    (setf position
	  (vec2
	   (x position)
	   (- (y position) (/ (y size) 2))))))

(defmethod render ((this blue-skill))
  (with-slots (position size fill-color flipped-p) this
    (let* ((frame (get-frame *anim-blue-skill* (real-time-seconds)))
	   (origin (keyframe-origin frame))
	   (end (keyframe-end frame)))
      (if flipped-p
	  (with-pushed-canvas ()
	    (scale-canvas -1 1)
	    (draw-image (vec2 (+ (- (x position)) (- (x size)) -80) (y position)) 'blue-skill
			:origin origin
			:width (- (x end) (x origin))
			:height (- (y end) (y origin))))
	  (draw-image (vec2 (- (x position) 80) (y position)) 'blue-skill
		      :origin origin
		      :width (- (x end) (x origin))
		      :height (- (y end) (y origin)))))))

(defmethod update-skill ((this blue-skill) player1 player2 ball delta-time)
  (with-slots (target speed collided-p) this
    (if (eql target 'player1)
	(progn
	  (move-skill this 'left delta-time)
	  (if (colliding-with this ball)
	      (let ((slip (make-instance 'slip :target ball)))
		(setf collided-p t)
		(push-effect ball slip))))
	(progn
	  (move-skill this 'right delta-time)
	  (if (colliding-with this ball)
	      (let ((slip (make-instance 'slip :target ball)))
		(setf collided-p t)
		(push-effect ball slip)))))))

(defmethod move-skill ((this blue-skill) direction delta-time &key)
  (with-slots (speed position) this
    (let ((real-speed (* speed delta-time)))
      (if (eql direction 'left)
	  (setf (x position) (- (x position) real-speed))
	  (setf (x position) (+ (x position) real-speed))))))
