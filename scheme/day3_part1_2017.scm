(define (spiral-distance n)
  (define (layer n)
    (if (= n 1)
        0
        (let* ((layer (ceiling (/ (- (sqrt n) 1) 2)))
               (max-in-layer (+ (* 2 layer) 1))
               (side-length (* 2 layer))
               (corner (- max-in-layer (* 3 layer)))
               (distance (min (modulo (- n corner) side-length)
                              (- side-length (modulo (- n corner) side-length)))))
          (+ layer distance))))
  (layer n))

(define (main)
  (let ((input (with-input-from-file "input.txt"
                  (lambda () (read)))))
    (display (spiral-distance input))
    (newline)))

(main)