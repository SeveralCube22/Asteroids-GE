(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

(defpackage :asteroids
  (:use :cl :sdl)
  (:export main))

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *ticks* 0)
(defparameter *ship-wing-angle* 15)
(defparameter *ship-nose-len* -15)
(defparameter *ship-wing-len* 25)
(defparameter *ship-max-velocity* 10.0)
(defparameter *ship-acceleration* 3)

(defun get-ticks ()
    (let* ((current (sdl:sdl-get-ticks))
           (delta (- current *ticks*)))
           
        (setf *ticks* current)
        (/ delta 1000.0)))

(defclass ship ()
    ((angle  :accessor angle :initform 90.0 :initarg :angle)
     
     (velocity-angle :accessor velocity-angle :initform 90.0 :initarg ::velocity-angle)

     (pos :accessor pos :initform (sdl:point :x 320 :y 240) :initarg :pos)
     
     (acceleration :accessor acceleration :initform 0.0 :initarg :acceleration)
    
     (velocity :accessor velocity :initform 0.0 :initarg :velocity)
     
     (rot-speed :accessor rot-speed :initform 250 :initarg :rot-speed)
     
     (turning :accessor turning :initform nil :initarg :turning)))

(defclass world ()
  ((ship :accessor ship :initform (make-instance 'ship) :initarg :ship)))

(defun point-from-angle-length (coord angle len)
    (sdl:point :x (+ (sdl:x coord) (* (* len (cos (* angle (/ pi 180)))) -1))
           :y (+ (sdl:y coord) (* len (sin (* angle (/ pi 180)))))))

(defun get-angle-in-range (angle)
    (cond ((> angle 360) (get-angle-in-range (- angle 360)))
          ((< angle 0) (get-angle-in-range (+ angle 360)))
          (t angle)))
        
(defmethod render ((ship ship))
        (let* ((nose (point-from-angle-length (pos ship) (angle ship) *ship-nose-len*))
              (left (point-from-angle-length nose (+ (angle ship) *ship-wing-angle*) *ship-wing-len*))
              (right (point-from-angle-length nose (- (angle ship) *ship-wing-angle*) *ship-wing-len*)))
            (sdl:draw-line (pos ship) nose :color sdl:*green*)
            (sdl:draw-line nose left :color sdl:*white*)
            (sdl:draw-line nose right :color sdl:*white*)
            (sdl:draw-line left right :color sdl:*white*)))


(defmethod thrust ((ship ship))
    (setf (acceleration ship) *ship-acceleration*)
    (let* ((v2x (+ (* (velocity ship) (cos (* (velocity-angle ship) (/ pi 180))))  (* (acceleration ship) (cos (* (angle ship) (/ pi 180))))))
           (v2y (+ (* (velocity ship) (sin (* (velocity-angle ship) (/ pi 180))))  (* (acceleration ship) (sin (* (angle ship) (/ pi 180))))))
           (velocity (sqrt (+ (* v2x v2x) (* v2y v2y))))
           (velocity-angle (atan (/ v2y v2x))))
            
            (cond ((> velocity *ship-max-velocity*) (setf (velocity ship) *ship-max-velocity*))
                  (t (setf (velocity ship) velocity)))
            (setf (velocity-angle ship) velocity-angle)))

(defmethod stop-thrust ((ship ship))
    (setf (acceleration ship) 0.0))

(defmethod rotate-left ((ship ship))
    (setf (rot-speed ship) ((lambda (x) 
              (cond ((<= x 0) x)
                        ((> x 0) (- 0 x)))) (rot-speed ship))))

(defmethod rotate-right ((ship ship))
    (setf (rot-speed ship) (abs (rot-speed ship))))

(defmethod update ((ship ship) delta-time)
  (cond ((turning ship) (setf (angle ship) (get-angle-in-range (+ (angle ship) (* (rot-speed ship) delta-time))))))
  (let* ((vx (* (velocity ship) (cos (* (velocity-angle ship) (/ pi 180))))) 
         (vy (* (velocity ship) (sin (* (velocity-angle ship) (/ pi 180)))))
         (dx (* vx delta-time))
         (dy (* vy delta-time))
         (x (+ dx (sdl:x (pos ship))))
         (y (+ dy (sdl:y (pos ship)))))
        
         (cond ((< x 0) (setf x 640))
               ((> x 640) (setf x 0)))

         (cond ((< y 0) (setf y 480))
               ((> y 480) (setf y 0)))

         (setf (pos ship) (sdl:point :x x :y y)))
  
  (check-collision)
  )

(defun main ()
    (sdl:with-init  ()
        (sdl:window *screen-width* *screen-height* :title-caption "Asteroids")
        (setf (sdl:frame-rate) 60)
        
      (let ((world (make-instance 'world)))
       (sdl:with-events ()
            (:quit-event () t)

            (:key-down-event (:key key)
              (case key
                  (:sdl-key-a 
                    (setf (turning (ship world)) t)
                    (rotate-left (ship world)))
                  (:sdl-key-d 
                    (setf (turning (ship world)) t)
                    (rotate-right (ship world)))  
                    
                  (:sdl-key-space
                        )
                    ))

            (:key-up-event (:key key)
              (case key
                  (:sdl-key-a 
                    (setf (turning (ship world)) nil))
                  
                  (:sdl-key-d
                    (setf (turning (ship world)) nil))))
                  
            (:idle ()
                (sdl:clear-display sdl:*black*)
                (update (ship world) (get-ticks))
                (render (ship world))
                (sdl:update-display))))))

(main)
