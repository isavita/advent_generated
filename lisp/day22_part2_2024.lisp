
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant +mod+ (ash 1 24))
(defconstant +num-steps+ 2000)
(defconstant +pattern-count+ (expt 19 4))

(defconstant +p19-1+ 19)
(defconstant +p19-2+ (* +p19-1+ +p19-1+))
(defconstant +p19-3+ (* +p19-1+ +p19-2+))

(declaim (ftype (function ((unsigned-byte 24)) (unsigned-byte 24)) next-secret))
(defun next-secret (s)
  (declare (type (unsigned-byte 24) s))
  (let ((mod-mask (1- +mod+)))
    (setf s (logand (logxor s (ash s 6)) mod-mask))
    (setf s (logand (logxor s (ash s -5)) mod-mask))
    (setf s (logand (logxor s (ash s 11)) mod-mask))
    s))

(declaim (ftype (function (fixnum fixnum fixnum fixnum) fixnum) encode-change4))
(defun encode-change4 (c1 c2 c3 c4)
  (declare (type fixnum c1 c2 c3 c4))
  (+ (+ c1 9)
     (* (+ c2 9) +p19-1+)
     (* (+ c3 9) +p19-2+)
     (* (+ c4 9) +p19-3+)))

(defun main ()
  (let* ((initials (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer t))
         (buyers-list nil)
         (global-sum (make-array +pattern-count+ :element-type 'fixnum :initial-element 0)))

    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil nil)
            while line
            when (not (string= line ""))
              do (vector-push-extend (parse-integer line) initials)))

    (loop for init-val across initials
          do (let* ((prices (make-array (1+ +num-steps+) :element-type 'fixnum))
                    (changes (make-array +num-steps+ :element-type 'fixnum))
                    (s init-val))
               (declare (type (unsigned-byte 24) s))
               (loop for j fixnum from 0 to +num-steps+
                     do (setf (aref prices j) (mod s 10))
                        (setf s (next-secret s)))
               (loop for j fixnum from 0 below +num-steps+
                     do (setf (aref changes j) (- (aref prices (1+ j)) (aref prices j))))
               (push (cons prices changes) buyers-list)))
    (setf buyers-list (nreverse buyers-list))

    (loop for (prices . changes) in buyers-list
          do (let ((local-price (make-array +pattern-count+ :element-type 'fixnum :initial-element -1)))
               (loop for i fixnum from 0 below (- +num-steps+ 3)
                     do (let ((c1 (aref changes i))
                              (c2 (aref changes (1+ i)))
                              (c3 (aref changes (+ i 2)))
                              (c4 (aref changes (+ i 3))))
                          (declare (type fixnum c1 c2 c3 c4))
                          (when (and (<= -9 c1 9)
                                     (<= -9 c2 9)
                                     (<= -9 c3 9)
                                     (<= -9 c4 9))
                            (let ((idx (encode-change4 c1 c2 c3 c4)))
                              (declare (type fixnum idx))
                              (when (< (aref local-price idx) 0)
                                (setf (aref local-price idx) (aref prices (+ i 4))))))))
               (loop for idx fixnum from 0 below +pattern-count+
                     do (let ((p (aref local-price idx)))
                          (declare (type fixnum p))
                          (when (>= p 0)
                            (incf (aref global-sum idx) p))))))

    (let ((max-val 0))
      (declare (type fixnum max-val))
      (loop for val across global-sum
            do (when (> val max-val)
                 (setf max-val val)))
      (print max-val))))

(main)
