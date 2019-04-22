;;;; utils.lisp
(in-package :com.eliasfeijo.pong)

;; Elapsed time in seconds
(defun real-time-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

;;; Position
(defgeneric (setf position-of) (vec2 positionable))

(defclass positionable ()
  ((position
    :initarg :position
    :reader position-of
    :initform (vec2 0 0))))

(defmethod (setf position-of) ((vec vec2) (this positionable))
  (setf (x (position-of this)) (x vec)
	(y (position-of this)) (y vec))
  (position-of this))

;;; Player
(defclass player (positionable)
  ((color
    :initarg :color
    :accessor color-of
    :initform (vec4 0.0 0.0 0.0 1))
   (size
    :initarg :size
    :accessor size-of
    :initform (vec2 100 20))
   (speed :initform 500 :accessor speed-of)
   (moving-left-p :initform nil :accessor moving-left-p)
   (moving-right-p :initform nil :accessor moving-right-p)))

;;; Ball
(defclass ball (positionable)
  ((color
    :initarg :color
    :accessor color-of
    :initform (vec4 0.0 0.0 0.0 1))
   (size
    :initarg :size
    :accessor size-of
    :initform (vec2 10 10))
   (speed :initform 50 :accessor speed-of)
   ;; The ball is always moving in one direction or another
   (moving-left-p :initform nil :accessor moving-left-p)
   (moving-down-p :initform nil :accessor moving-down-p)))
   
