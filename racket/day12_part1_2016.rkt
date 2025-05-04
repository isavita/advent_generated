
#lang racket

(define (parse-arg s)
  (or (string->number s) (string->symbol s)))

(define (parse-instruction line)
  (match (string-split line)
    [(list cmd x)   (list (string->symbol cmd) (parse-arg x))]
    [(list cmd x y) (list (string->symbol cmd) (parse-arg x) (parse-arg y))]
    [_              (error "Invalid instruction format:" line)]))

(define (get-value registers arg)
  (if (symbol? arg)
      (hash-ref registers arg)
      arg))

(define (run-program instructions)
  (define registers (make-hash '((a . 0) (b . 0) (c . 0) (d . 0))))
  (define num-instructions (vector-length instructions))

  (let loop ([ip 0])
    (if (or (< ip 0) (>= ip num-instructions))
        (hash-ref registers 'a)
        (let ([instr (vector-ref instructions ip)])
          (match instr
            [(list 'cpy x y)
             (hash-set! registers y (get-value registers x))
             (loop (+ ip 1))]
            [(list 'inc x)
             (hash-set! registers x (+ (get-value registers x) 1))
             (loop (+ ip 1))]
            [(list 'dec x)
             (hash-set! registers x (- (get-value registers x) 1))
             (loop (+ ip 1))]
            [(list 'jnz x y)
             (if (zero? (get-value registers x))
                 (loop (+ ip 1))
                 (loop (+ ip (get-value registers y))))]
            [_ (error "Unknown parsed instruction:" instr)])))))

(define (main)
  (define lines (file->lines "input.txt"))
  (define parsed-instructions (list->vector (map parse-instruction lines)))
  (displayln (run-program parsed-instructions)))

(main)
