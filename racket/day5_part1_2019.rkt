#lang racket

(define (get-mode instruction position)
  (modulo (quotient instruction (expt 10 (+ position 1))) 10))

(define (get-param program pointer mode)
  (if (= mode 0)
      (vector-ref program (vector-ref program pointer))
      (vector-ref program pointer)))

(define (run-program program input)
  (define output 0)
  (define pointer 0)
  (define len (vector-length program))
  (let loop ()
    (when (< pointer len)
      (define instruction (vector-ref program pointer))
      (define opcode (modulo instruction 100))

      (cond
        [(or (= opcode 1) (= opcode 2))
         (define param1 (get-param program (+ pointer 1) (get-mode instruction 1)))
         (define param2 (get-param program (+ pointer 2) (get-mode instruction 2)))
         (define result (if (= opcode 1) (+ param1 param2) (* param1 param2)))
         (vector-set! program (vector-ref program (+ pointer 3)) result)
         (set! pointer (+ pointer 4))
         (loop)]

        [(= opcode 3)
         (vector-set! program (vector-ref program (+ pointer 1)) input)
         (set! pointer (+ pointer 2))
         (loop)]

        [(= opcode 4)
         (set! output (get-param program (+ pointer 1) (get-mode instruction 1)))
         (set! pointer (+ pointer 2))
         (loop)]

        [(= opcode 99)
         output]

        [else
         (error "Unknown opcode" opcode)]))))

(define (main)
  (define data (file->string "input.txt"))
  (define str-program (map string->number (string-split (string-trim data) ",")))
  (define program (apply vector str-program)) ; Corrected line
  (displayln (run-program program 1)))

(main)