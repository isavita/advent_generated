
#lang racket

(struct item (cost damage armor) #:transparent)
(struct character (hit-points damage armor) #:transparent)

(define (parse-stat line)
  (string->number (second (string-split line ": "))))

(define (turns-needed attacker-damage defender-hp)
  (ceiling (/ defender-hp attacker-damage)))

(define (player-wins player boss)
  (define player-damage (max 1 (- (character-damage player) (character-armor boss))))
  (define boss-damage (max 1 (- (character-damage boss) (character-armor player))))
  (<= (turns-needed player-damage (character-hit-points boss))
      (turns-needed boss-damage (character-hit-points player))))

(define weapons
  (list (item 8 4 0)
        (item 10 5 0)
        (item 25 6 0)
        (item 40 7 0)
        (item 74 8 0)))

(define armors
  (list (item 0 0 0)
        (item 13 0 1)
        (item 31 0 2)
        (item 53 0 3)
        (item 75 0 4)
        (item 102 0 5)))

(define rings
  (list (item 0 0 0)
        (item 25 1 0)
        (item 50 2 0)
        (item 100 3 0)
        (item 20 0 1)
        (item 40 0 2)
        (item 80 0 3)))

(define (main)
  (define boss
    (with-input-from-file "input.txt"
      (lambda ()
        (define hp (parse-stat (read-line)))
        (define dmg (parse-stat (read-line)))
        (define arm (parse-stat (read-line)))
        (character hp dmg arm))))

  (define max-cost 0)

  (for* ([w weapons]
         [a armors]
         [i (in-range (length rings))]
         [j (in-range (+ i 1) (length rings))])

    (define r1 (list-ref rings i))
    (define r2 (list-ref rings j))

    (define player-damage (+ (item-damage w) (item-damage r1) (item-damage r2)))
    (define player-armor (+ (item-armor a) (item-armor r1) (item-armor r2)))
    (define current-cost (+ (item-cost w) (item-cost a) (item-cost r1) (item-cost r2)))

    (define player (character 100 player-damage player-armor))

    (when (and (not (player-wins player boss)) (> current-cost max-cost))
      (set! max-cost current-cost)))

  (printf "~a\n" max-cost))

(module+ main
  (main))
