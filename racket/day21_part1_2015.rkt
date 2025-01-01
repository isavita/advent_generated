
#lang racket

(define (parse-input filename)
  (let ((lines (file->lines filename)))
    (define (parse-line line)
      (string->number (cadr (string-split line ": "))))
    (list (parse-line (first lines))
          (parse-line (second lines))
          (parse-line (third lines)))))

(define (simulate-fight player-hp player-damage player-armor boss-hp boss-damage boss-armor)
  (let loop ((player-hp player-hp) (boss-hp boss-hp) (player-turn? #t))
    (cond
      ((<= boss-hp 0) #t) ; Player wins
      ((<= player-hp 0) #f) ; Player loses
      (else
       (if player-turn?
           (let ((damage (max 1 (- player-damage boss-armor))))
             (loop player-hp (- boss-hp damage) #f))
           (let ((damage (max 1 (- boss-damage player-armor))))
             (loop (- player-hp damage) boss-hp #t)))))))

(define weapons
  (list (list 8 4 0)
        (list 10 5 0)
        (list 25 6 0)
        (list 40 7 0)
        (list 74 8 0)))

(define armors
  (list (list 0 0 0) ; No armor
        (list 13 0 1)
        (list 31 0 2)
        (list 53 0 3)
        (list 75 0 4)
        (list 102 0 5)))

(define rings
  (list (list 0 0 0) ; No ring
        (list 25 1 0)
        (list 50 2 0)
        (list 100 3 0)
        (list 20 0 1)
        (list 40 0 2)
        (list 80 0 3)))

(define (solve filename)
  (let* ((boss-stats (parse-input filename))
         (boss-hp (first boss-stats))
         (boss-damage (second boss-stats))
         (boss-armor (third boss-stats))
         (min-gold (make-parameter #f)))
    (for* ((weapon weapons)
           (armor armors)
           (ring1 rings)
           (ring2 rings))
      (when (not (equal? ring1 ring2))
        (let* ((total-cost (+ (first weapon) (first armor) (first ring1) (first ring2)))
               (total-damage (+ (second weapon) (second armor) (second ring1) (second ring2)))
               (total-armor (+ (third weapon) (third armor) (third ring1) (third ring2))))
          (when (simulate-fight 100 total-damage total-armor boss-hp boss-damage boss-armor)
            (when (or (not (min-gold)) (< total-cost (min-gold)))
              (min-gold total-cost))))))
    (min-gold)))

(display (solve "input.txt"))
(newline)
