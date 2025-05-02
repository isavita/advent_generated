
#lang racket/base

(require racket/string
         racket/list
         racket/file)

(define (build-bridge components available-indices current-port current-strength)
  (for/fold ([max-strength current-strength])
            ([idx available-indices])
    (let ([comp (vector-ref components idx)])
      (if (or (= current-port (first comp)) (= current-port (second comp)))
          (let* ([next-port (if (= current-port (first comp)) (second comp) (first comp))]
                 [new-strength (+ current-strength (first comp) (second comp))]
                 [remaining-indices (filter (lambda (i) (not (= i idx))) available-indices)])
            (let ([recursive-strength (build-bridge components remaining-indices next-port new-strength)])
              (max max-strength recursive-strength)))
          max-strength))))

(define (main)
  (define components-list
    (map (lambda (line) (map string->number (string-split line "/")))
         (file->lines "input.txt")))

  (define components (list->vector components-list))

  (define initial-indices (build-list (vector-length components) values))

  (define max-strength (build-bridge components initial-indices 0 0))

  (printf "~a~n" max-strength))

(main)
