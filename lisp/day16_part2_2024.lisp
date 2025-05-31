
(defpackage :advent-of-code
  (:use :cl)
  (:export :main))

(in-package :advent-of-code)

(defconstant +dx+ '(-1 0 1 0))
(defconstant +dy+ '(0 1 0 -1))
(defconstant +infinity+ most-positive-fixnum)

(defstruct (node (:constructor %make-node (x y d cost)))
  x y d cost)

(defun make-min-heap ()
  (make-array 10 :fill-pointer 0 :adjustable t))

(defun heap-empty-p (heap)
  (zerop (fill-pointer heap)))

(defun heap-parent (i) (ash (1- i) -1))
(defun heap-left-child (i) (1+ (ash i 1)))
(defun heap-right-child (i) (1+ (heap-left-child i)))

(defun heap-push (heap node)
  (vector-push-extend node heap)
  (let ((i (1- (fill-pointer heap))))
    (loop while (and (> i 0)
                     (< (node-cost (aref heap i))
                        (node-cost (aref heap (heap-parent i))))) do
      (rotatef (aref heap i) (aref heap (heap-parent i)))
      (setf i (heap-parent i)))))

(defun heap-pop (heap)
  (when (heap-empty-p heap) (return-from heap-pop nil))
  (let ((val (aref heap 0)))
    (when (> (fill-pointer heap) 1)
      (setf (aref heap 0) (aref heap (1- (fill-pointer heap)))))
    (vector-pop heap)
    (let ((i 0)
          (size (fill-pointer heap)))
      (loop
        (let ((left (heap-left-child i))
              (right (heap-right-child i))
              (smallest i))
          (when (and (< left size)
                     (< (node-cost (aref heap left))
                        (node-cost (aref heap smallest))))
            (setf smallest left))
          (when (and (< right size)
                     (< (node-cost (aref heap right))
                        (node-cost (aref heap smallest))))
            (setf smallest right))
          (when (= smallest i) (return))
          (rotatef (aref heap i) (aref heap smallest))
          (setf i smallest))))
    val))

(defun solve ()
  (let* (grid n m sx sy ex ey dist heap best used rev vis)
    (with-open-file (in "input.txt" :direction :input)
      (let ((lines (loop for line = (read-line in nil nil)
                         while line collect line)))
        (setf n (length lines))
        (setf m (length (first lines)))
        (setf grid (make-array n :initial-contents lines))))

    (loop for i from 0 below n do
      (loop for j from 0 below m do
        (let ((char (char (aref grid i) j)))
          (cond ((char= char #\S) (setf sx i sy j))
                ((char= char #\E) (setf ex i ey j))))))

    (setf dist (make-array (list n m 4) :initial-element +infinity+))
    (setf (aref dist sx sy 1) 0)
    (setf heap (make-min-heap))
    (heap-push heap (%make-node sx sy 1 0))

    (loop while (not (heap-empty-p heap)) do
      (block dijkstra-iteration
        (let* ((u (heap-pop heap))
               (ux (node-x u))
               (uy (node-y u))
               (ud (node-d u))
               (ucost (node-cost u)))

          (when (< (aref dist ux uy ud) ucost)
            (return-from dijkstra-iteration))

          (when (and (= ux ex) (= uy ey))
            (return-from dijkstra-iteration))

          (loop for delta-d in '(1 3) do
            (let* ((nd (mod (+ ud delta-d) 4))
                   (nc (+ ucost 1000)))
              (when (< nc (aref dist ux uy nd))
                (setf (aref dist ux uy nd) nc)
                (heap-push heap (%make-node ux uy nd nc)))))

          (let* ((nx (+ ux (nth ud +dx+)))
                 (ny (+ uy (nth ud +dy+)))
                 (nc (+ ucost 1)))
            (when (and (>= nx 0) (< nx n)
                       (>= ny 0) (< ny m)
                       (char/= (char (aref grid nx) ny) #\#))
              (when (< nc (aref dist nx ny ud))
                (setf (aref dist nx ny ud) nc)
                (heap-push heap (%make-node nx ny ud nc))))))))

    (setf best +infinity+)
    (loop for d from 0 below 4 do
      (setf best (min best (aref dist ex ey d))))

    (setf used (make-array (list n m) :initial-element nil))
    (setf rev nil)
    (setf vis (make-array (list n m 4) :initial-element nil))

    (loop for d from 0 below 4 do
      (when (= (aref dist ex ey d) best)
        (push (%make-node ex ey d (aref dist ex ey d)) rev)
        (setf (aref vis ex ey d) t)))

    (loop while rev do
      (let* ((u (pop rev))
             (ux (node-x u))
             (uy (node-y u))
             (ud (node-d u))
             (ucost (node-cost u)))

        (setf (aref used ux uy) t)

        (loop for prev-d-offset in '(1 3) do
          (let ((prev-d (mod (+ ud prev-d-offset) 4)))
            (when (= (aref dist ux uy prev-d) (- ucost 1000))
              (unless (aref vis ux uy prev-d)
                (setf (aref vis ux uy prev-d) t)
                (push (%make-node ux uy prev-d (- ucost 1000)) rev)))))

        (let* ((px (- ux (nth ud +dx+)))
               (py (- uy (nth ud +dy+))))
          (when (and (>= px 0) (< px n)
                     (>= py 0) (< py m)
                     (char/= (char (aref grid px) py) #\#))
            (when (= (aref dist px py ud) (- ucost 1))
              (unless (aref vis px py ud)
                (setf (aref vis px py ud) t)
                (push (%make-node px py ud (- ucost 1)) rev)))))))

    (let ((cnt 0))
      (loop for i from 0 below n do
        (loop for j from 0 below m do
          (when (and (aref used i j) (char/= (char (aref grid i) j) #\#))
            (incf cnt))))
      (princ cnt))))

(defun main ()
  (solve))

(main)
