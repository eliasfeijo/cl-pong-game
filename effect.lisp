;;;; effect.lisp
(in-package :com.eliasfeijo.pong)

(defclass effect ()
  ((time-started :initform (real-time-seconds) :reader time-started)
   (duration :reader duration-of)
   (target :initarg :target)))

(defgeneric start-effect (effect))

(defgeneric stop-effect (effect))

(defclass burning (effect)
  ((duration :initform 2.0)))

(defmethod start-effect ((this burning))
  (with-slots (target) this
    (setf (speed-of target) 2000)))

(defmethod stop-effect ((this burning))
  (with-slots (target) this
    (setf (speed-of target) 500)))
