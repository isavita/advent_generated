#lang racket

(define (read-input)
  (define input (file->string "input.txt"))
  (define parts (string-split input))
  (define players (string->number (list-ref parts 0)))
  (define last-marble (string->number (list-ref parts 6)))
  (values players last-marble))

(define (marble-mania players last-marble)
  (define scores (make-vector players 0))
  (define marbles (list 0))
  (define current-index 0)

  (for ([marble (in-range 1 (+ last-marble 1))])
    (if (= (modulo marble 23) 0)
        (let ([player (modulo marble players)])
          (vector-set! scores player (+ (vector-ref scores player) marble))
          (define remove-index (modulo (- current-index 7) (length marbles)))
          (define removed-marble (list-ref marbles remove-index))
          (vector-set! scores player (+ (vector-ref scores player) removed-marble))
          (set! marbles (append (take marbles remove-index) (drop marbles (+ remove-index 1))))
          (set! current-index remove-index))
        (begin
          (set! current-index (modulo (+ current-index 2) (length marbles)))
          (set! marbles (append (take marbles current-index)
                                (list marble)
                                (drop marbles current-index))))))

  (apply max (vector->list scores)))

(define (main)
  (define-values (players last-marble) (read-input))
  (define high-score (marble-mania players last-marble))
  (printf "Winning Elf's score: ~a\n" high-score))

(main)