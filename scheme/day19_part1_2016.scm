(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (read))))

(define (find-winning-elf total-elves)
  (let loop ((highest-power-of-two 1))
    (if (<= (* highest-power-of-two 2) total-elves)
        (loop (* highest-power-of-two 2))
        (+ (* 2 (- total-elves highest-power-of-two)) 1))))

(define (main)
  (let ((total-elves (read-input "input.txt")))
    (display (find-winning-elf total-elves))))

(main)