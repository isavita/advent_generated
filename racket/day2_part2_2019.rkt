
#lang racket

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (let ((line (read-line)))
        (if (eof-object? line)
            '()
            (map string->number (string-split line ",")))))))

(define (run-intcode program)
  (let loop ((memory program) (ip 0))
    (let ((opcode (list-ref memory ip)))
      (cond
        ((= opcode 99) (list-ref memory 0))
        ((= opcode 1)
         (let ((pos1 (list-ref memory (+ ip 1)))
               (pos2 (list-ref memory (+ ip 2)))
               (pos3 (list-ref memory (+ ip 3))))
           (loop (list-update memory pos3 (+ (list-ref memory pos1) (list-ref memory pos2))) (+ ip 4))))
        ((= opcode 2)
         (let ((pos1 (list-ref memory (+ ip 1)))
               (pos2 (list-ref memory (+ ip 2)))
               (pos3 (list-ref memory (+ ip 3))))
           (loop (list-update memory pos3 (* (list-ref memory pos1) (list-ref memory pos2))) (+ ip 4))))
        (else (error "Invalid opcode" opcode))))))

(define (list-update lst index new-value)
  (if (>= index (length lst))
      lst
      (let loop ((i 0) (acc '()))
        (cond
          ((= i index) (append acc (list new-value) (drop lst (add1 i))))
          ((>= i (length lst)) acc)
          (else (loop (add1 i) (append acc (list (list-ref lst i)))))))))

(define (solve-part1 program)
  (let ((modified-program (list-update (list-update program 1 12) 2 2)))
    (run-intcode modified-program)))

(define (solve-part2 program target)
  (for* ((noun (in-range 0 100))
         (verb (in-range 0 100)))
    (let ((modified-program (list-update (list-update program 1 noun) 2 verb)))
      (when (= (run-intcode modified-program) target)
        (displayln (+ (* 100 noun) verb))
        (exit)))))

(define (main)
  (let ((program (read-input "input.txt")))
    (displayln (solve-part1 program))
    (solve-part2 program 19690720)))

(main)
