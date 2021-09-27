(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

(load "/home/vmanam/Desktop/Repo/Asteroids/neuralnet.lisp")

(defpackage :asteroids
  (:use :cl :sdl)
  (:export main))

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *ticks* 0)
(defparameter *ship-wing-angle* 15)
(defparameter *ship-nose-len* -15)
(defparameter *ship-wing-len* 25)
(defparameter *ship-radius* 10)
(defparameter *ship-max-velocity* 500.0) ; weird values because don't know how to properly setup delta-time
(defparameter *ship-acceleration* 100)
(defparameter *ship-acceleration-scaling* .90)
(defparameter *asteroid-max-states* 3)
(defparameter *max-asteroids-spawn* 3)
(defparameter *asteroid-init-speed* 70)
(defparameter *asteroid-init-radius* 30)
(defparameter *bullet-radius* 3)
(defparameter *bullet-velocity* 300)
(defparameter *shoot-delay* 15)

(defparameter *time* 500)
(defparameter *population* 4)
(defparameter *generations* 3)
(defparameter *mutatation-rate* .2)

(defun get-ticks ()
    (let* ((current (sdl:sdl-get-ticks))
           (delta (- current *ticks*)))
           
        (setf *ticks* current)
        (/ delta 1000.0)))

(defclass ship ()
    ((angle  :accessor angle :initform 90.0 :initarg :angle)
     (velocity-angle :accessor velocity-angle :initform 0.0 :initarg ::velocity-angle)
     (pos :accessor pos :initform (sdl:point :x 320 :y 240) :initarg :pos)
     (acceleration :accessor acceleration :initform 0.0 :initarg :acceleration)
     (velocity :accessor velocity :initform 0.0 :initarg :velocity)
     (rot-speed :accessor rot-speed :initform 300 :initarg :rot-speed)
     (turning :accessor turning :initform nil :initarg :turning)
     (thrusting :accessor thrusting :initform nil :initarg :thrusting)
     (shooting :accessor shooting :initform nil :initarg :shooting)))

(defclass asteroid ()
    ((velocity-angle :accessor velocity-angle :initform (+ 0 (random 340)) :initarg :velocity-angle)
     (velocity :accessor velocity :initarg :velocity)
     (pos :accessor pos :initarg :pos)
     (state :accessor state :initarg :state) 
     (radius :accessor radius :initarg :radius)))

(defclass bullet ()
    ((velocity-angle :accessor velocity-angle :initarg :velocity-angle)
     (velocity :accessor velocity :initform *bullet-velocity* :initarg :velocity)
     (pos :accessor pos :initarg :pos)
     (radius :accessor radius :initform *bullet-radius* :initarg :radius)))

(defclass world ()
  ((ship :accessor ship :initform (make-instance 'ship) :initarg :ship)
   (asteroids :accessor asteroids :initform nil :initarg :asteroids)
   (bullets :accessor bullets :initform nil :initarg :bullets)
   (game-over :accessor game-over :initform nil :initarg :game-over)
   (neural-net :accessor neural-net :initform (make-instance 'asteroids-neural-net:neural-net :inNodes 3 :hiddenNodes 20 :outNodes 4) :initarg :neural-net)
   (delay :accessor delay :initform 0)
   (fitness :accessor fitness :initform 0)
   (lifetime :accessor lifetime :initform 0)
   (shots-fired :accessor shots-fired :initform 1)
   (shots-hit :accessor shots-hit :initform 0)
   (curr-time :accessor curr-time :initform 0)
   (paused :accessor paused :initform nil)))

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

(defmethod render ((asteroid asteroid))
  (sdl:draw-circle (pos asteroid) (radius asteroid) :color sdl:*white*))

(defmethod render ((bullet bullet))
  (sdl:draw-circle (pos bullet) (radius bullet) :color sdl:*white*))


(defmethod thrust ((ship ship))
    (setf (acceleration ship) (+ (* *ship-acceleration-scaling* (acceleration ship)) 1))
    (cond ((> (acceleration ship) *ship-acceleration*) (setf (acceleration ship) *ship-acceleration*)))

    (let* ((v2x (+ (* (velocity ship) (cos (* (velocity-angle ship) (/ pi 180))))  (* (acceleration ship) (cos (* (angle ship) (/ pi 180))))))
           (v2y (+ (* (velocity ship) (sin (* (velocity-angle ship) (/ pi 180))))  (* (acceleration ship) (sin (* (angle ship) (/ pi 180))))))
           (velocity (sqrt (+ (* v2x v2x) (* v2y v2y))))
           (velocity-angle (cond ((not (= v2x 0)) (* (atan (/ v2y v2x)) (/ 180 pi))))))
            
            (cond ((> velocity *ship-max-velocity*) (setf (velocity ship) *ship-max-velocity*))
                  (t (setf (velocity ship) velocity)))

            (cond ((= v2x 0) (<= v2y 0) (setf velocity-angle 270))
                  ((= v2x 0) (>= v2y 0) (setf velocity-angle 90))
                  ((and (> v2x 0) (< v2y 0)) (setf velocity-angle (+ velocity-angle 360)))
                  ((and (< v2x 0) (>= v2y 0)) (setf velocity-angle (+ velocity-angle 180)))
                  ((and (< v2x 0) (< v2y 0))  (setf velocity-angle (+ velocity-angle 180))))

            (setf (velocity-angle ship) velocity-angle)))

(defmethod stop-thrust ((ship ship))
    (setf (acceleration ship) 0.0))

(defmethod shoot ((world world))
    (cond ((shooting (ship world))
        (cond ((= (mod (delay world) *shoot-delay*) 0)
                (setf (bullets world) 
                  (cons
                    (make-instance 'bullet :velocity-angle (angle (ship world)) :pos (pos (ship world))) 
                    (bullets world)))))
        (incf (delay world) 1)
        (incf (shots-fired world) 1)
        (cond ((> (delay world) *shoot-delay*) (setf (delay world) 0))))))

(defmethod rotate-left ((ship ship))
    (setf (rot-speed ship) ((lambda (x) 
              (cond ((<= x 0) x)
                        ((> x 0) (- 0 x)))) (rot-speed ship))))

(defmethod rotate-right ((ship ship))
    (setf (rot-speed ship) (abs (rot-speed ship))))

(defmethod update ((ship ship) delta-time)
  (cond ((turning ship) (setf (angle ship) (get-angle-in-range (+ (angle ship) (* (rot-speed ship) delta-time))))))
  (cond ((thrusting ship) (thrust ship))
        (t (stop-thrust ship)))

  (let* ((vx (* (velocity ship) (cos (* (velocity-angle ship) (/ pi 180))))) 
         (vy (* (velocity ship) (sin (* (velocity-angle ship) (/ pi 180)))))
         (dx (* vx delta-time))
         (dy (* vy delta-time))
         (x (+ (sdl:x (pos ship)) dx))
         (y (- (sdl:y (pos ship)) dy)))
        
        (cond ((< x 0) (setf x 640))
               ((> x 640) (setf x 0)))

        (cond ((< y 0) (setf y 480))
               ((> y 480) (setf y 0)))

        (setf (pos ship) (sdl:point :x x :y y)))

  (render ship))

(defmethod update ((asteroid asteroid) delta-time)
   (let* ((vx (* (velocity asteroid) (cos (* (velocity-angle asteroid) (/ pi 180))))) 
          (vy (* (velocity asteroid) (sin (* (velocity-angle asteroid) (/ pi 180)))))
          (dx (* vx delta-time))
          (dy (* vy delta-time))
          (x (+ (sdl:x (pos asteroid)) dx))
          (y (- (sdl:y (pos asteroid)) dy)))
        
         (cond ((< x 0) (setf x 640))
               ((> x 640) (setf x 0)))

        (cond ((< y 0) (setf y 480))
               ((> y 480) (setf y 0)))

        (setf (pos asteroid) (sdl:point :x x :y y)))
        
  (render asteroid))


(defmethod update ((bullet bullet) delta-time)
   (let* ((vx (* (velocity bullet) (cos (* (velocity-angle bullet) (/ pi 180))))) 
          (vy (* (velocity bullet) (sin (* (velocity-angle bullet) (/ pi 180)))))
          (dx (* vx delta-time))
          (dy (* vy delta-time))
          (x (+ (sdl:x (pos bullet)) dx))
          (y (- (sdl:y (pos bullet)) dy)))
        
        (setf (pos bullet) (sdl:point :x x :y y)))
        
  (render bullet))

(defmethod update ((world world) delta-time)
  (incf (lifetime world) 1)
  (update (ship world) delta-time)
  (shoot world)
  (dolist (bullet (bullets world))
    (update bullet delta-time)
      (cond ((< (sdl:x (pos bullet)) 0) (setf (bullets world) (remove bullet (bullets world))))
               ((> (sdl:x (pos bullet)) 640) (setf (bullets world) (remove bullet (bullets world)))))

        (cond ((< (sdl:y (pos bullet)) 0) (setf (bullets world) (remove bullet (bullets world))))
               ((> (sdl:y (pos bullet)) 480) (setf (bullets world) (remove bullet (bullets world))))))

  (dolist (asteroid (asteroids world))
           (update asteroid delta-time)
           (cond ((> (expt (+ *ship-radius* (radius asteroid)) 2) (+ (expt (- (sdl:x (pos (ship world))) (sdl:x (pos asteroid))) 2)
                                                                     (expt (- (sdl:y (pos (ship world))) (sdl:y (pos asteroid))) 2))) 
                      (setf (game-over world) t)))
           (dolist (bullet (bullets world))
                    (cond ((> (expt (+ (radius bullet) (radius asteroid)) 2) (+ (expt (- (sdl:x (pos bullet)) (sdl:x (pos asteroid))) 2)
                                                                     (expt (- (sdl:y (pos bullet)) (sdl:y (pos asteroid))) 2)))
                      (incf (shots-hit world) 1) 
                      (setf (asteroids world) (remove asteroid (asteroids world)))
                      (setf (bullets world) (remove bullet (bullets world)))
                      (cond ((< (state asteroid) 3)
                                (setf (asteroids world) (append (asteroids world) (create-asteroids (mod (+ (state asteroid) 1) 3)
                                                                                                    (+ (state asteroid) 1) (pos asteroid) nil world)))))))))
                                                                                                    
           (let ((num-asteroids 0))
                (dolist (asteroid (asteroids world))
                        (cond ((= (state asteroid) 1) (incf num-asteroids))))
                
                (setf (asteroids world) (append (asteroids world) (create-asteroids (- *max-asteroids-spawn* num-asteroids) 1 nil t world)))))

(defmethod see ((world world))
  (let ((direction nil)
        (minDis 801))

      (dolist (asteroid (asteroids world))
        (let* ((xDiff (- (sdl:x (pos asteroid)) (sdl:x (pos (ship world)))))
               (yDiff (- (sdl:y (pos asteroid)) (sdl:y (pos (ship world)))))
               (distance (sqrt (+ (expt xDiff 2) 
                                 (expt yDiff 2))))
               (angle (cond ((not (= xDiff 0)) (* (atan (/ yDiff xDiff)) (/ 180 pi))))))
              
            (cond ((= xDiff 0) (<= yDiff 0) (setf angle 270))
                  ((= xDiff 0) (>= yDiff 0) (setf angle 90))
                  ((and (> xDiff 0) (< yDiff 0)) (setf angle (+ angle 360)))
                  ((and (< xDiff 0) (> yDiff 0)) (setf angle (+ angle 180)))
                  ((and (< xDiff 0) (< yDiff 0))  (setf angle (+ angle 180))))

            (cond ((< distance minDis) 
                    (setf direction (list (/ distance 1d0) (/ angle 1d0)))))))
      (cons (/ (acceleration (ship world)) 1d0) direction)))

(defmethod calculate-fitness ((world world))
  (setf (fitness world) (+ (* (shots-hit world) 10.0) (lifetime world)  (expt (/ (shots-hit world) (shots-fired world)) 2.0))))
                                                                                                    

(defun create-asteroids (num state pos randomp world)
     (cond (randomp 
              (setf pos 
                        (let ((x 0)
                              (y 0))
                            (do ((i 0)) 
                                ((not (= i 0)))
                                
                              (setf x (random 640))
                              (setf y (random 480))
                              (cond ((and (not (= x (sdl:x (pos (ship world))))) (not (= y (sdl:y (pos (ship world)))))) (setf i 1))))

                            (sdl:point :x x :y y)))))

     (let ((asteroids nil))
        (dotimes (number num)
            (setf asteroids (cons (make-instance 'asteroid :pos pos :state state :velocity (* *asteroid-init-speed* (/ state 1.3)) :radius (/ *asteroid-init-radius* state)) 
                                    asteroids)))
        asteroids))

(defmethod init-asteroids ((world world))
   (setf (asteroids world) (create-asteroids *max-asteroids-spawn* 1 nil t world)))

(defmethod play-game ((world world))
  (cond ((not (paused world))
                        (sdl:clear-display sdl:*black*)
                        (let ((out (asteroids-neural-net:feed (neural-net world) (see world) 3)))

                            (cond ((> (first out) .8d0) (setf (thrusting (ship world)) t)) 
                                  (t (setf (thrusting (ship world)) nil)))
                            (cond ((> (second out) .8d0) (setf (shooting (ship world)) t)) 
                                  (t (setf (shooting (ship world)) nil)))
                            (cond ((> (third out) .8d0) 
                                      (setf (turning (ship world)) t)
                                      (rotate-left (ship world)))
                                  ((> (fourth out) .8d0) 
                                      (setf (turning (ship world)) t)
                                      (rotate-right (ship world))) 
                                  (t (setf (turning (ship world)) nil))))
                        (incf (curr-time world))
                        (update world (get-ticks))
                        )))

(defmethod alive ((world world))
  (and (not (game-over world)) (< (curr-time world) *time*)))

(defun calculate-total-fit (population)
  (let ((total-fit 0.0))
    (dolist (p population)
      (setf total-fit (+ total-fit (fitness p))))
    total-fit))

(defun select-random-player (population total-fit)
  (let ((rand (random total-fit)))
    (dolist (p population)
      (cond ((< rand (fitness p)) (return-from select-random-player p))
            (t (setf rand (- rand (fitness p))))))))

(defun main ()
    (sdl:with-init  ()
        (sdl:window *screen-width* *screen-height* :title-caption "Asteroids")
        (setf (sdl:frame-rate) 60)
        
      (let* ((world (make-instance 'world))
             (init t)
             (curr-pop 0)
             (curr-gen 0)
             (population (list world))
             (best-fitness -1)
             (best-player nil))
       (init-asteroids world)
       (asteroids-neural-net:init (neural-net world))
       (sdl:with-events ()
            (:quit-event () t)

            (:key-down-event (:key key)
              (case key
                  (:sdl-key-a 
                    (setf (turning (ship world)) t)
                    (rotate-right (ship world)))
                  (:sdl-key-d 
                    (setf (turning (ship world)) t)
                    (rotate-left (ship world)))  
                    
                  (:sdl-key-space
                    (setf (thrusting (ship world)) t))

                  (:sdl-key-p
                    (setf (paused world) (not (paused world))))

                  (:sdl-key-f
                    (setf (shooting (ship world)) t))
                    
                  (:sdl-key-l 
                    (let ((net (asteroids-neural-net:read-net "/home/vmanam/Desktop/Repo/Asteroids/out.txt"))
                          (new-world (make-instance 'world)))
                      (setf (neural-net new-world) net)
                      (setf world new-world)
                      (init-asteroids world)))))

            (:key-up-event (:key key)
              (case key
                  (:sdl-key-a 
                    (setf (turning (ship world)) nil))
                  
                  (:sdl-key-d
                    (setf (turning (ship world)) nil))
                    
                  (:sdl-key-space 
                    (setf (thrusting (ship world)) nil))
                    
                  (:sdl-key-f
                    (setf (shooting (ship world)) nil))))
                  
            (:idle ()
                (cond ((alive world) (play-game world))
                    
                      ((and (not (alive world)) (< curr-pop *population*))
                        (cond ((> (calculate-fitness world) best-fitness)
                                (asteroids-neural-net:save (neural-net world) "/home/vmanam/Desktop/Repo/Asteroids/out.txt") 
                                (setf best-fitness (calculate-fitness world))
                                (setf best-player (neural-net world))))
                        (sdl:clear-display sdl:*black*)
                        (incf curr-pop)
                        (cond ((and init (< curr-pop *population*)) 
                                  (setf world (make-instance 'world))
                                  (setf population (append population (list world)))
                                  (init-asteroids world)
                                  (asteroids-neural-net:init (neural-net world)))
                              ((<= curr-pop *population*) (setf world (nth (- curr-pop 1) population)) (init-asteroids world))))

                      ((and (not (alive world)) (>= curr-pop *population*) (< curr-gen *generations*))
                        (sdl:clear-display sdl:*black*)
                        (let ((best-world (make-instance 'world))
                               (new-pop nil))

                          (setf (neural-net best-world) best-player)
                          (setf new-pop (list best-world))
                          (do ((i 1 (+ i 1)))
                                ((>= i *population*))
                              (cond ((< i (/ *population* 2)) 
                                      (let* ((p (select-random-player population (calculate-total-fit population)))
                                             (net (asteroids-neural-net:mutate (neural-net p) *mutatation-rate*))
                                             (new-p (make-instance 'world)))
                                        (setf (neural-net new-p) net)
                                        (setf new-pop (append new-pop (list new-p)))))
                  
                                    (t
                                      (let* ((p1 (select-random-player population (calculate-total-fit population)))
                                             (p2 (select-random-player population (calculate-total-fit population)))
                                             (child (asteroids-neural-net:crossover (neural-net p1) (neural-net p2)))
                                             (child-mutated (asteroids-neural-net:mutate child *mutatation-rate*))
                                             (p (make-instance 'world)))  
                                         
                                        (setf (neural-net p) child-mutated)
                                        (setf new-pop (append new-pop (list p)))))))
                          (setf population new-pop)
                          (setf init nil)
                          (incf curr-gen)
                          (setf curr-pop 1)
                          (setf world (first population))
                          (init-asteroids world)))
                          
                      ((and (not (alive world)) (>= curr-pop *population*) (>= curr-gen *generations*)) (sdl:clear-display sdl:*black*)))

                (sdl:update-display))))))

(main)
