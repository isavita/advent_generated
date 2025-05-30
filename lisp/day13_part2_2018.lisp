
(defstruct cart
  x y direction turns)

(defun read-lines (filename)
  (with-open-file (s filename)
    (loop for line = (read-line s nil :eof)
          until (eq line :eof)
          collect line)))

(defun parse-input (lines)
  (let* ((width (length (car lines)))
         (height (length lines))
         (tracks (make-array (list height width) :element-type 'character))
         (carts nil))
    (loop for y from 0 for line in lines do
      (loop for x from 0 for char across line do
        (setf (aref tracks y x)
              (case char
                (#\> (push (make-cart :x x :y y :direction #\> :turns 0) carts) #\-)
                (#\< (push (make-cart :x x :y y :direction #\< :turns 0) carts) #\-)
                (#\^ (push (make-cart :x x :y y :direction #\^ :turns 0) carts) #\|)
                (#\v (push (make-cart :x x :y y :direction #\v :turns 0) carts) #\|)
                (t char)))))
    (values tracks (nreverse carts))))

(defun move-cart (cart tracks)
  (with-slots (x y direction) cart
    (case direction
      (#\> (incf x))
      (#\< (decf x))
      (#\^ (decf y))
      (#\v (incf y))))
  (let ((track-char (aref tracks (cart-y cart) (cart-x cart))))
    (case track-char
      (#\+ (turn-cart cart))
      (#\/ (change-direction cart #\/))
      (#\\ (change-direction cart #\\)))))

(defun turn-cart (cart)
  (with-slots (direction turns) cart
    (case (mod turns 3)
      (0 (setf direction (case direction (#\> #\^) (#\< #\v) (#\^ #\<) (#\v #\>))))
      (2 (setf direction (case direction (#\> #\v) (#\< #\^) (#\^ #\>) (#\v #\<)))))
    (incf turns)))

(defun change-direction (cart track)
  (with-slots (direction) cart
    (case track
      (#\/ (setf direction (case direction (#\> #\^) (#\< #\v) (#\^ #\>) (#\v #\<))))
      (#\\ (setf direction (case direction (#\> #\v) (#\< #\^) (#\^ #\<) (#\v #\>)))))))

(defun find-crashed-cart (current-cart carts)
  (loop for other-cart in carts
        when (and (not (eq current-cart other-cart))
                  (= (cart-x current-cart) (cart-x other-cart))
                  (= (cart-y current-cart) (cart-y other-cart)))
          do (return other-cart)))

(defun main ()
  (multiple-value-bind (tracks carts) (parse-input (read-lines "input.txt"))
    (loop while (> (length carts) 1) do
      (setf carts (sort carts #'(lambda (a b)
                                  (if (= (cart-y a) (cart-y b))
                                      (< (cart-x a) (cart-x b))
                                      (< (cart-y a) (cart-y b))))))
      (let ((crashed-this-tick (list)))
        (loop for cart in carts do
          (unless (member cart crashed-this-tick)
            (move-cart cart tracks)
            (let ((other-crashed-cart (find-crashed-cart cart carts)))
              (when other-crashed-cart
                (push cart crashed-this-tick)
                (push other-crashed-cart crashed-this-tick)))))
        (setf carts (remove-if #'(lambda (c) (member c crashed-this-tick)) carts))))
    (format t "~a,~a~%" (cart-x (car carts)) (cart-y (car carts)))))

(main)
