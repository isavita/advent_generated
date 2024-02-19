
#lang racket

(define (get-value program pos mode)
  (if (= mode 0)
      (vector-ref program (vector-ref program pos))
      (vector-ref program pos)))

(define (run-program program input)
  (define output 0)
  (define i 0)
  (let loop ()
    (define opcode (remainder (vector-ref program i) 100))
    (define modes (quotient (vector-ref program i) 100))
    (define param1-mode (remainder modes 10))
    (set! modes (quotient modes 10))
    (define param2-mode (remainder modes 10))
    
    (cond
      [(= opcode 1)
       (define p1 (get-value program (+ i 1) param1-mode))
       (define p2 (get-value program (+ i 2) param2-mode))
       (define p3 (vector-ref program (+ i 3)))
       (vector-set! program p3 (+ p1 p2))
       (set! i (+ i 4))]
      
      [(= opcode 2)
       (define p1 (get-value program (+ i 1) param1-mode))
       (define p2 (get-value program (+ i 2) param2-mode))
       (define p3 (vector-ref program (+ i 3)))
       (vector-set! program p3 (* p1 p2))
       (set! i (+ i 4))]
      
      [(= opcode 3)
       (vector-set! program (vector-ref program (+ i 1)) input)
       (set! i (+ i 2))]
      
      [(= opcode 4)
       (set! output (get-value program (+ i 1) param1-mode))
       (display output)
       (newline)
       (set! i (+ i 2))]
      
      [(= opcode 5)
       (define p1 (get-value program (+ i 1) param1-mode))
       (define p2 (get-value program (+ i 2) param2-mode))
       (if (not (= p1 0))
           (set! i p2)
           (set! i (+ i 3)))]
      
      [(= opcode 6)
       (define p1 (get-value program (+ i 1) param1-mode))
       (define p2 (get-value program (+ i 2) param2-mode))
       (if (= p1 0)
           (set! i p2)
           (set! i (+ i 3)))]
      
      [(= opcode 7)
       (define p1 (get-value program (+ i 1) param1-mode))
       (define p2 (get-value program (+ i 2) param2-mode))
       (define p3 (vector-ref program (+ i 3)))
       (if (< p1 p2)
           (vector-set! program p3 1)
           (vector-set! program p3 0))
       (set! i (+ i 4))]
      
      [(= opcode 8)
       (define p1 (get-value program (+ i 1) param1-mode))
       (define p2 (get-value program (+ i 2) param2-mode))
       (define p3 (vector-ref program (+ i 3)))
       (if (= p1 p2)
           (vector-set! program p3 1)
           (vector-set! program p3 0))
       (set! i (+ i 4))]
      
      [(= opcode 99)
       #t]
      
      [else
       (error "Invalid opcode")])
    
    (if (not (= opcode 99))
        (loop)
        #f)) ; Added an else expression to fix the error
  
  output)

(define input-file "input.txt")

(define program
  (with-input-from-file input-file
    (lambda ()
      (let loop ((line (read-line)))
        (if (eof-object? line)
            '()
            (append (map string->number (string-split line ",")) (loop (read-line))))))))

(run-program (list->vector program) 5)
