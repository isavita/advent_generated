
#lang racket

(define (parse-particle line)
  (define parts (regexp-split #px"[<>, ]+" line))
  (define position (list (string->number (list-ref parts 1))
                         (string->number (list-ref parts 2))
                         (string->number (list-ref parts 3))))
  (define velocity (list (string->number (list-ref parts 5))
                         (string->number (list-ref parts 6))
                         (string->number (list-ref parts 7))))
  (define acceleration (list (string->number (list-ref parts 9))
                             (string->number (list-ref parts 10))
                             (string->number (list-ref parts 11))))
  (list position velocity acceleration))

(define (update-particle particle)
  (define position (first particle))
  (define velocity (second particle))
  (define acceleration (third particle))
  
  (define new-velocity (map + velocity acceleration))
  (define new-position (map + position new-velocity))
  
  (list new-position new-velocity acceleration))

(define (manhattan-distance position)
  (apply + (map abs position)))

(define (simulate particles ticks)
  (define (simulate-ticks particles remaining-ticks)
    (if (zero? remaining-ticks)
        particles
        (simulate-ticks (map update-particle particles) (sub1 remaining-ticks))))
  (simulate-ticks particles ticks))

(define (find-closest-particle particles)
  (define distances (map (lambda (p) (manhattan-distance (first p))) particles))
  (define min-distance (apply min distances))
  (define closest-particle (index-of distances min-distance))
  closest-particle)

(define (main)
  (define input (file->string "input.txt"))
  (define particles (map parse-particle (string-split input "\n" #:trim? #t)))
  (define final-particles (simulate particles 1000)) ; Simulate for 1000 ticks
  (define closest-particle (find-closest-particle final-particles))
  (printf "The closest particle is: ~a\n" closest-particle))

(main)
