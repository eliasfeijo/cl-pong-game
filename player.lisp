;;;; player.lisp
(in-package :com.eliasfeijo.pong)

(define-image 'player-red "images/player-red.png")
(define-image 'player-green "images/player-green.png")
(define-image 'player-blue "images/player-blue.png")

(defclass player (positionable renderable actor)
  ((color
    :initarg :color
    :accessor color-of
    :initform nil)
   (size
    :initarg :size
    :accessor size-of
    :initform (vec2 20 100))
   (speed :initform 500 :accessor speed-of)
   (moving-up-p :initform nil :accessor moving-up-p)
   (moving-down-p :initform nil :accessor moving-down-p)
   (time-last-skill :initform (real-time-seconds) :accessor time-last-skill)
   (image :reader image-of)
   (flipped-p :initarg :flipped-p :initform nil)))

(defmethod (setf color-of) (value (this player))
  (with-slots (image color) this
    (cond
      ((string-equal (name-of value) 'red)
       (setf image 'player-red))
      ((string-equal (name-of value) 'green)
       (setf image 'player-green))
      ((string-equal (name-of value) 'blue)
       (setf image 'player-blue)))
    (setf color value)))

(defun center-of (player)
  (let ((position (position-of player))
	(size (size-of player)))
    (vec2
     (+ (x position) (/ (x size) 2))
     (+ (y position) (/ (y size) 2)))))

(defun move (player direction delta-time)
  (let ((real-speed (* (speed-of player) delta-time)))
    (case direction
      (down
       (if (> (+ (y (position-of player)) (y (size-of player))) 0)
	   (setf (y (position-of player)) (- (y (position-of player)) real-speed))))
      (up
       (if (< (y (position-of player)) *canvas-height*)
	   (setf (y (position-of player)) (+ (y (position-of player)) real-speed)))))))

(defun update-player (player delta-time)
  (update-effects player)
  (cond
    ((moving-down-p player) (move player 'down delta-time))
    ((moving-up-p player) (move player 'up delta-time))))

(defmethod render ((this player))
  (with-slots (image flipped-p position size) this
    (if flipped-p
	(with-pushed-canvas ()
	  (scale-canvas -1 1)
	  (draw-image (vec2 (+ (- (x position)) (- (x size))) (y position)) image :width (x size) :height (y size)))
	(draw-image position image :width (x size) :height (y size)))))


