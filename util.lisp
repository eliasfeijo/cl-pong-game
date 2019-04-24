;;;; utils.lisp
(in-package :com.eliasfeijo.pong)

(defvar *canvas-width* 800)
(defvar *canvas-height* 600)

(defvar *black* (vec4 0 0 0 1))

;; Elapsed time in seconds
(defun real-time-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))

;;; Rendering
(defgeneric render (object)
  (:method (object) (declare (ignore object))))

(defclass renderable () ())

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

;;; Keyboard

(defgeneric press-key (keyboard key)
  (:method (keyboard key)))

(defgeneric release-key (keyboard key)
  (:method (keyboard key)))
