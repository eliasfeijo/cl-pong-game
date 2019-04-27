;;;; utils.lisp
(in-package :com.eliasfeijo.pong)

(defvar *canvas-width* 800)
(defvar *canvas-height* 600)
(defvar *canvas-origin* (vec2 0 0))
(defvar *canvas-center* (vec2 (/ *canvas-width* 2) (/ *canvas-height* 2)))

(defvar *black* (vec4 0 0 0 1))
(defvar *white* (vec4 1 1 1 1))



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


;;; Color

(defclass color ()
  ((name :initarg :name :accessor name-of)
   (description :initarg :description :accessor description-of)
   (skill :reader skill-of)
   (value :initform (vec4 0 0 0 1) :initarg :value :accessor value-of)))

(defmethod initialize-instance :after ((this color) &key)
  (with-slots (name skill) this
    (cond
      ((string-equal name 'red)
       (setf skill 'red-skill)))))



(defparameter *list-color*
  (list
   (make-instance 'color
		  :name "Red"
		  :description "Burn your foe!"
		  :value (vec4 1 0 0 1))
   (make-instance 'color
		  :name "Green"
		  :description "Slow down your foe!"
		  :value (vec4 0 1 0 1))
   (make-instance 'color
		  :name "Blue"
		  :description "Increases the ball's speed."
		  :value (vec4 0 0 1 1))))



;;; Collision
(defun colliding-with (origin target)
  (if
   (and
    (<=
     (x (position-of origin))
     (+ (x (position-of target)) (x (size-of target))))
    (>=
     (+ (x (position-of origin)) (x (size-of origin)))
     (x (position-of target))))
   ;; Ball x is inside
   (cond 
     ((and
       (<=
	(y (position-of origin))
	(+ (y (position-of target)) (y (size-of target))))
       (>=
	(+ (y (position-of origin)) (y (size-of origin)))
	(y (position-of target)))) t)) nil))


;;; Actor

(defclass actor ()
  ((effects :initform (make-array 5 :fill-pointer 0) :accessor effects-of)))

(defgeneric update-effects (actor))
(defgeneric push-effect (actor effect))

(defmethod update-effects ((this actor))
  (loop
     for i from 0
     for effect across (effects-of this)
     do
       (let ((time-elapsed (- (real-time-seconds) (time-started effect))))
	 (if (> time-elapsed (duration-of effect))
	     (progn
	       (stop-effect effect)
	       (vector-pop-position* (effects-of this) i))))))

(defmethod push-effect ((this actor) effect)
  (with-slots (effects) this
    (vector-push effect effects)
    (start-effect effect)))
