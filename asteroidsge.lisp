
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

(defpackage :asteroids
  (:use :cl :sdl)
  (:export main))

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *ticks* 0)

(defclass ship ()
    ((angle  :accessor angle :initform 90.0 :initarg :angle)
     
     (pos :accessor pos :initform (sdl:point :x 320 :y 240) :initarg :pos)
     
     (acceleration :accessor acceleration :initform 3.0 :initarg :acceleration)
    
     (velocity :accessor velocity :initform '(0 . 0) :initarg :velocity)
     
     (ship-length :accessor ship-length :initform 25 :initarg :ship-length)
     
     (rot-speed :accessor rot-speed :initform 3 :initarg :rot-speed)
     
     (turning :accessor turning :initform nil :initarg :turning)))

(defclass world ()
  ((ship :accessor ship :initform (make-instance 'ship) :initarg :ship)))

(defun point-from-angle-length (coord angle len)
    (sdl:point :x (+ (* (* len (cos (* angle (/ pi 180)))) -1) (sdl:x coord))
           :y (+ (* (* len (sin (* angle (/ pi 180)))) -1) (sdl:y coord))))

(defun get-angle-in-range (angle)
    (cond ((> angle 360) (get-angle-in-range (- angle 360)))
          ((< angle 0) (get-angle-in-range (+ angle 360)))
          (t angle)))


(defmethod render ((ship ship))
        (let* ((nose (point-from-angle-length (pos ship) (angle ship) (ship-length ship)))
              (func (cond ((> (angle ship) 90) #'+)
                          ((< (angle ship) 90) #'-)
                          (t (lambda (x y) x))))
              (left (point-from-angle-length nose 
                            (get-angle-in-range (funcall func 250 (angle ship))) (ship-length ship)))
              (right (point-from-angle-length nose 
                            (get-angle-in-range (funcall func 290 (angle ship))) (ship-length ship))))
      
            (sdl:draw-line nose left :color sdl:*white*)
            (sdl:draw-line nose right :color sdl:*white*)
            (sdl:draw-line left right :color sdl:*white*)))

(defmethod update ((ship ship))
  (cond ((turning ship) (setf (angle ship) (+ (rot-speed ship) (angle ship))))))

(defun main ()
    (sdl:with-init  ()
        (sdl:window *screen-width* *screen-height* :title-caption "Asteroids")
        (setf (sdl:frame-rate) 60)
        
      (let ((world (make-instance 'world)))
       (sdl:with-events ()
            (:quit-event () t)

            (:key-down-event (:key key)
              (case key
                  (:sdl-key-a (setf (turning (ship world)) t))))

            (:key-up-event (:key key)
              (case key
                  (:sdl-key-a (setf (turning (ship world)) nil))))
            (:idle ()
                (sdl:clear-display sdl:*black*)
                (update (ship world))
                (render (ship world))
                (sdl:update-display))))))

(main)

