(defpackage :asteroids-neural-net
  (:use :cl)
  (:export neural-net
           init
           feed
           activate
           sigmoid
           randomize
           save
           read-net
           mutate
           crossover))

(in-package :asteroids-neural-net)


(ql:quickload :magicl)
(ql:quickload :parse-number)
(ql:quickload :cl-ppcre)

(defclass neural-net ()
  ((inNodes :accessor inNodes :initarg :inNodes)
   (hiddenNodes :accessor hiddenNodes :initarg :hiddenNodes)
   (outNodes :accessor outNodes :initarg :outNodes)
   (inMatrix :accessor inMatrix)
   (hiddenMatrix :accessor hiddenMatrix)
   (outMatrix :accessor outMatrix)))

(defmethod init ((net neural-net))
  (let* ((in (incf (inNodes net)))
        (hid (hiddenNodes net))
        (hh (+ hid 1))
        (out (+ (outNodes net) 1)))
        
        (setf (inMatrix net) (magicl:from-list (randomize (* in hid)) (list hid in)))
        (setf (hiddenMatrix net) (magicl:from-list (randomize (* hid hh)) (list hid hh)))
        (setf (outMatrix net) (magicl:from-list (randomize (* hh out)) (list out hh)))))

(defmethod feed ((net neural-net) input row)
  (setf input (append input (list 1.0)))
  (incf row)
  (let* ((i (activate (magicl:@ (inMatrix net) (magicl:from-list input (list row 1)))))
         (hi (magicl:vstack (list i (magicl:from-list (list 1d0) '(1 1)))))
         (hhi (magicl:vstack (list (activate (magicl:@ (hiddenMatrix net) hi)) (magicl:from-list (list 1d0) '(1 1)))))
         (ho (activate (magicl:@ (outMatrix net) hhi)))
         (out nil))

        (do ((i 0 (+ i 1)))
            ((>= i (outNodes net)))
          
          (setf out (cons (magicl:tref ho i 0) out)))
        out))

(defmethod mutate ((net neural-net) rate) 
  (mutate-matrix (inMatrix net) rate)
  (mutate-matrix (hiddenMatrix net) rate)
  (mutate-matrix (outMatrix net) rate)
  (let ((res (make-instance 'neural-net :inNodes (inNodes net) :hiddenNodes (hiddenNodes net) :outNodes (outNodes net))))
    (setf (inMatrix res) (inMatrix net))
    (setf (hiddenMatrix res) (hiddenMatrix net))
    (setf (outMatrix res) (outMatrix net))
    res))

(defmethod crossover ((n1 neural-net) (n2 neural-net))
  (let ((child (make-instance 'neural-net :inNodes (inNodes n1) :hiddenNodes (hiddenNodes n1) :outNodes (outNodes n1))))
     (setf (inMatrix child) (crossover-matrix (inMatrix n1) (inMatrix n2)))
     (setf (hiddenMatrix child) (crossover-matrix (hiddenMatrix n1) (hiddenMatrix n2)))
     (setf (outMatrix child) (crossover-matrix (outMatrix n1) (outMatrix n2)))
     child))

(defmethod save ((net neural-net) filename)
  (with-open-file (out filename :direction :output
                                :if-exists :overwrite
                                :if-does-not-exist :create)
  (format out "~A~%" (inNodes net))
  (format out "~A~%" (hiddenNodes net))
  (format out "~A~%" (outNodes net))

  (let ((in (get-values (inMatrix net)))
        (hidden (get-values (hiddenMatrix net)))
        (outM (get-values (outMatrix net))))
        
      (dolist (num in)
        (format out "~A " num))
      (format out "~%")

      (dolist (num hidden)
        (format out "~A " num))
      (format out "~%")  

      (dolist (num outM)
        (format out "~A " num)))))


(defun read-net (filename)
  (with-open-file (in filename
                      :direction :input)
    (let* ((data (loop :for line := (read-line in nil)
                           :while line
                           :collect (mapcar #'parse-number:parse-number
                                            (cl-ppcre:split "\\s+" line))))
              (net nil))
           (format t "~A~%" (second (first data)))
           (setf net (make-instance 'neural-net :inNodes (first (first data))
                                      :hiddenNodes (first (second data))
                                      :outNodes (first (third data))))

           (setf (inMatrix net) (magicl:from-list (fourth data) (list (hiddenNodes net) (inNodes net))))
           (setf (hiddenMatrix net) (magicl:from-list (fifth data) (list (hiddenNodes net) (+ (hiddenNodes net) 1))))
           (setf (outMatrix net) (magicl:from-list (sixth data) (list (+ (outNodes net) 1)  (+ (hiddenNodes net) 1))))
           net)))

(defun get-values (matrix)
   (let* ((shape (magicl:shape matrix))
         (row (first shape))
         (col (second shape))
         (val nil))
    
    (do ((i 0 (+ i 1)))
        ((>= i row))
    
      (do ((j 0 (+ j 1)))
          ((>= j col))
    
        (cond ((and (= i 0) (= j 0)) (setf val (list (magicl:tref matrix i j))))
              (t (setf val (append val (list (magicl:tref matrix i j))))))))
    val))

(defun mutate-matrix (matrix rate)
  (let* ((shape (magicl:shape matrix))
         (row (first shape))
         (col (second shape)))
      (do ((i 0 (+ i 1)))
          ((>= i row))
    
        (do ((j 0 (+ j 1)))
              ((>= j col))
        
          (cond ((< (random 1.0) rate) (setf (magicl:tref matrix i j) (nn-random)))) ))))

(defun crossover-matrix (m1 m2)
  (let* ((shape (magicl:shape m1))
         (row (first shape))
         (col (second shape))
         (rR (random row))
         (rC (random col))
         (res (magicl:rand (list row col))))
        
    (do ((i 0 (+ i 1)))
        ((>= i row))
    
      (do ((j 0 (+ j 1)))
          ((>= j col))
        
        (cond ((and (< i rR) (< j rC)) (setf (magicl:tref res i j) (magicl:tref m1 i j)))
              (t (setf (magicl:tref res i j) (magicl:tref m2 i j))))))
      res))

(defun activate (matrix)
  (let* ((shape (magicl:shape matrix))
         (row (first shape))
         (col (second shape)))
    
    (do ((i 0 (+ i 1)))
        ((>= i row))
    
      (do ((j 0 (+ j 1)))
          ((>= j col))
    
        (setf (magicl:tref matrix i j) (sigmoid (magicl:tref matrix i j))))))
    matrix)

(defun sigmoid (x)
  (/ 1 (+ 1 (exp (- x)))))

(defun nn-random ()
  (/ (+ (- (random 2.0)) 1.0) 1d0))

(defun randomize (num)
  (let ((res nil))
    (dotimes (number num)
      (setf res (cons (nn-random) res)))
      
    res))



