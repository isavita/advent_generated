
(defpackage :advent-of-code
  (:use :cl)
  (:export :main))

(in-package :advent-of-code)

(defconstant +clean+    :clean)
(defconstant +weakened+ :weakened)
(defconstant +infected+ :infected)
(defconstant +flagged+  :flagged)

(defconstant +directions+ '((0 -1) (1 0) (0 1) (-1 0)))

(defstruct carrier
  (grid       (make-hash-table :test 'equal))
  (x          0 :type integer)
  (y          0 :type integer)
  (direction  0 :type (mod 4))
  (infections 0 :type integer))

(declaim (inline turn-left turn-right reverse-direction))

(defun turn-left (c)
  (setf (carrier-direction c) (mod (+ (carrier-direction c) 3) 4)))

(defun turn-right (c)
  (setf (carrier-direction c) (mod (+ (carrier-direction c) 1) 4)))

(defun reverse-direction (c)
  (setf (carrier-direction c) (mod (+ (carrier-direction c) 2) 4)))

(declaim (inline burst))
(defun burst (c)
  (let* ((grid (carrier-grid c))
         (cx (carrier-x c))
         (cy (carrier-y c))
         (coords (list cx cy))
         (current-node (gethash coords grid +clean+)))

    (ecase current-node
      (#.+clean+
       (turn-left c)
       (setf (gethash coords grid) +weakened+))
      (#.+weakened+
       (setf (gethash coords grid) +infected+)
       (incf (carrier-infections c)))
      (#.+infected+
       (turn-right c)
       (setf (gethash coords grid) +flagged+))
      (#.+flagged+
       (reverse-direction c)
       (setf (gethash coords grid) +clean+)))

    (let* ((dir-vec (nth (carrier-direction c) +directions+))
           (dx (first dir-vec))
           (dy (second dir-vec)))
      (incf (carrier-x c) dx)
      (incf (carrier-y c) dy))))

(defun make-initial-carrier (initial-map)
  (let* ((rows (length initial-map))
         (start-coord (floor rows 2))
         (carrier (make-carrier :x start-coord :y start-coord)))
    (loop for row from 0 below rows
          for line = (nth row initial-map)
          do (loop for col from 0 below (length line)
                   when (char= (char line col) #\#)
                     do (setf (gethash (list col row) (carrier-grid carrier)) +infected+)))
    carrier))

(defun read-input (filepath)
  (with-open-file (s filepath :direction :input :if-does-not-exist :error)
    (loop for line = (read-line s nil nil)
          while line
          collect line)))

(defun main ()
  (let* ((initial-map (read-input "input.txt"))
         (carrier (make-initial-carrier initial-map)))
    (loop for i from 0 below 10000000
          do (burst carrier))
    (format t "~a~%" (carrier-infections carrier))))

(main)
