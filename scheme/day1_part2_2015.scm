(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (read-line))))

(define (calculate-floor instructions)
  (define (helper instr floor pos)
    (cond
      ((null? instr) floor)
      ((equal? (car instr) #\() (helper (cdr instr) (+ floor 1) (+ pos 1)))
      ((equal? (car instr) #\)) 
       (if (= (+ floor -1) -1) pos (helper (cdr instr) (- floor 1) (+ pos 1))))
      (else (helper (cdr instr) floor (+ pos 1)))))
  (helper (string->list instructions) 0 0))

(define (find-basement-position instructions)
  (define (helper instr floor pos)
    (if (null? instr)
        #f
        (begin
          (if (= floor -1)
              pos
              (if (equal? (car instr) #\()
                  (helper (cdr instr) (+ floor 1) (+ pos 1))
                  (helper (cdr instr) (- floor 1) (+ pos 1)))))))
  (helper (string->list instructions) 0 0))

(define instructions (read-input "input.txt"))
(define final-floor (calculate-floor instructions))
(define basement-position (find-basement-position instructions))

(display "Final Floor: ")
(display final-floor)
(newline)
(display "First Basement Position: ")
(display basement-position)
(newline)