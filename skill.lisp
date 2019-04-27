;;;; skill.lisp
(in-package :com.eliasfeijo.pong)

(defparameter +delay-red-skill+ 3.0)
(defparameter +delay-green-skill+ 2.0)
(defparameter +delay-blue-skill+ 2.5)


(defclass skill (positionable renderable)
  ((target :initarg :target :initform nil)
   (size :initarg :size :initform nil :accessor size-of)
   (speed :initarg :speed :initform nil :accessor speed-of)
   (fill-color :initarg :fill-color :initform nil :accessor fill-color-of)
   (collided-p :initform nil :reader collided-p)))

(defgeneric update-skill (skill player1 player2 ball delta-time))
(defgeneric move-skill (skill direction delta-time))

(defmethod render ((this skill))
  (with-slots (position size fill-color) this
    (draw-rect position (x size) (y size) :fill-paint fill-color)))

;;; Red skill

(defclass red-skill (skill)
  ((size :initform (vec2 50 50))
   (speed :initform 400)
   (fill-color :initform (vec4 1 0 0 1))))

(defmethod initialize-instance :after ((this red-skill) &key)
  (with-slots (position size) this
    (setf position
	  (vec2
	   (x position)
	   (- (y position) (/ (y size) 2))))))

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

(defmethod move-skill ((this red-skill) direction delta-time)
  (with-slots (speed position) this
    (let ((real-speed (* speed delta-time)))
      (if (eql direction 'left)
	  (setf (x position) (- (x position) real-speed))
	  (setf (x position) (+ (x position) real-speed))))))

;;; Green skill

(defclass green-skill (skill)
  ((size :initform (vec2 30 10))
   (speed :initform 600)
   (fill-color :initform (vec4 0 1 0 1))))

(defmethod initialize-instance :after ((this green-skill) &key)
  (with-slots (position size) this
    (setf position
	  (vec2
	   (x position)
	   (- (y position) (/ (y size) 2))))))

(defmethod update-skill ((this green-skill) player1 player2 ball delta-time)
  (with-slots (target speed collided-p) this
    (if (eql target 'player1)
	(progn
	  (move-skill this 'left delta-time)
	  (if (colliding-with this player1)
	      (let ((slow (make-instance 'slow :target player1)))
		(setf collided-p t)
		(push-effect player1 slow))))
	(progn
	  (move-skill this 'right delta-time)
	  (if (colliding-with this player2)
	      (let ((slow (make-instance 'slow :target player2)))
		(setf collided-p t)
		(push-effect player2 slow)))))))

(defmethod move-skill ((this green-skill) direction delta-time)
  (with-slots (speed position) this
    (let ((real-speed (* speed delta-time)))
      (if (eql direction 'left)
	  (setf (x position) (- (x position) real-speed))
	  (setf (x position) (+ (x position) real-speed))))))

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

(defmethod move-skill ((this blue-skill) direction delta-time)
  (with-slots (speed position) this
    (let ((real-speed (* speed delta-time)))
      (if (eql direction 'left)
	  (setf (x position) (- (x position) real-speed))
	  (setf (x position) (+ (x position) real-speed))))))
