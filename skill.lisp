;;;; skill.lisp
(in-package :com.eliasfeijo.pong)

(defclass skill (positionable renderable)
  ((target :initarg :target :initform nil)
   (size :initarg :size :initform nil :accessor size-of)
   (speed :initarg :speed :initform nil :accessor speed-of)
   (fill-color :initarg :fill-color :initform nil :accessor fill-color-of)
   (collided-p :initform nil :reader collided-p)))

(defgeneric update-skill (skill player1 player2 delta-time))
(defgeneric move-skill (skill direction delta-time))

(defmethod render ((this skill))
  (with-slots (position size fill-color) this
    (draw-rect position (x size) (y size) :fill-paint fill-color)))

(defparameter +delay-red-skill+ 3.0)

(defclass red-skill (skill)
  ((size :initform (vec2 50 50))
   (speed :initform 400)
   (fill-color :initform (vec4 1 0 0 1))))

(defmethod update-skill ((this red-skill) player1 player2 delta-time)
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

(defmethod move-skill ((this red-skill) direction delta-time)
  (with-slots (speed position) this
    (let ((real-speed (* speed delta-time)))
      (if (eql direction 'left)
	  (setf (x position) (- (x position) real-speed))
	  (setf (x position) (+ (x position) real-speed))))))
