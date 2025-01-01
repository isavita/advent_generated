
#lang racket

(struct ingredient (name capacity durability flavor texture calories) #:mutable)

(define (read-ingredients filename)
  (with-input-from-file filename
    (λ ()
      (let loop ([ingredients '()])
        (let ([line (read-line)])
          (cond
            [(eof-object? line) (reverse ingredients)]
            [else
             (let* ([parts (string-split line)]
                    [capacity (string->number (substring (list-ref parts 2) 0 (- (string-length (list-ref parts 2)) 1)))]
                    [durability (string->number (substring (list-ref parts 4) 0 (- (string-length (list-ref parts 4)) 1)))]
                    [flavor (string->number (substring (list-ref parts 6) 0 (- (string-length (list-ref parts 6)) 1)))]
                    [texture (string->number (substring (list-ref parts 8) 0 (- (string-length (list-ref parts 8)) 1)))]
                    [calories (string->number (list-ref parts 10))])
               (loop (cons (ingredient (list-ref parts 0) capacity durability flavor texture calories) ingredients)))]))))))

(define (calculate-calories ingredients teaspoons)
  (apply + (map (λ (ing tsp) (* (ingredient-calories ing) tsp)) ingredients teaspoons)))

(define (score ingredients teaspoons)
  (let* ([capacity (apply + (map (λ (ing tsp) (* (ingredient-capacity ing) tsp)) ingredients teaspoons))]
         [durability (apply + (map (λ (ing tsp) (* (ingredient-durability ing) tsp)) ingredients teaspoons))]
         [flavor (apply + (map (λ (ing tsp) (* (ingredient-flavor ing) tsp)) ingredients teaspoons))]
         [texture (apply + (map (λ (ing tsp) (* (ingredient-texture ing) tsp)) ingredients teaspoons))])
    (max 0 (* (max 0 capacity) (max 0 durability) (max 0 flavor) (max 0 texture)))))

(define (find-max-score ingredients total-teaspoons target-calories)
  (let loop ([index 0] [remaining total-teaspoons] [teaspoons '()] [max-score 0])
    (cond
      [(= index (- (length ingredients) 1))
       (let* ([new-teaspoons (append teaspoons (list remaining))]
              [current-calories (calculate-calories ingredients new-teaspoons)]
              [current-score (if (= current-calories target-calories)
                                 (score ingredients new-teaspoons)
                                 0)])
         (max max-score current-score))]
      [else
       (let loop2 ([i 0] [current-max-score max-score])
         (cond
           [(> i remaining) current-max-score]
           [else
            (let ([new-max-score (loop (+ index 1) (- remaining i) (append teaspoons (list i)) current-max-score)])
              (loop2 (+ i 1) (max current-max-score new-max-score)))]))])))

(let ([ingredients (read-ingredients "input.txt")])
  (printf "~a\n" (find-max-score ingredients 100 500)))
