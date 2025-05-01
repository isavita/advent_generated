
#lang racket
(require racket/struct)
(require racket/file)
(define min-mana +inf.0)
(struct game-state (player-hp player-mana boss-hp boss-damage shield-timer poison-timer recharge-timer mana-spent) #:transparent)
(define (simulate state player-turn?)
  (when (< (game-state-mana-spent state) min-mana)
    (let ((state-after-hp-loss (if player-turn?
                                   (struct-copy game-state state [player-hp (sub1 (game-state-player-hp state))])
                                   state)))
      (when (> (game-state-player-hp state-after-hp-loss) 0)
        (let ((state-after-effects (struct-copy game-state state-after-hp-loss
                                                [player-mana (+ (game-state-player-mana state-after-hp-loss) (if (> (game-state-recharge-timer state-after-hp-loss) 0) 101 0))]
                                                [boss-hp (- (game-state-boss-hp state-after-hp-loss) (if (> (game-state-poison-timer state-after-hp-loss) 0) 3 0))]
                                                [shield-timer (max 0 (sub1 (game-state-shield-timer state-after-hp-loss)))]
                                                [poison-timer (max 0 (sub1 (game-state-poison-timer state-after-hp-loss)))]
                                                [recharge-timer (max 0 (sub1 (game-state-recharge-timer state-after-hp-loss)))])))
          (when (<= (game-state-boss-hp state-after-effects) 0)
            (set! min-mana (min min-mana (game-state-mana-spent state-after-effects))))
          (unless (<= (game-state-boss-hp state-after-effects) 0)
            (if player-turn?
                (begin
                  (when (>= (game-state-player-mana state-after-effects) 53)
                    (simulate (struct-copy game-state state-after-effects
                                           [player-mana (- (game-state-player-mana state-after-effects) 53)]
                                           [mana-spent (+ (game-state-mana-spent state-after-effects) 53)]
                                           [boss-hp (- (game-state-boss-hp state-after-effects) 4)])
                              #f))
                  (when (>= (game-state-player-mana state-after-effects) 73)
                    (simulate (struct-copy game-state state-after-effects
                                           [player-mana (- (game-state-player-mana state-after-effects) 73)]
                                           [mana-spent (+ (game-state-mana-spent state-after-effects) 73)]
                                           [boss-hp (- (game-state-boss-hp state-after-effects) 2)]
                                           [player-hp (+ (game-state-player-hp state-after-effects) 2)])
                              #f))
                  (when (and (>= (game-state-player-mana state-after-effects) 113)
                             (= (game-state-shield-timer state-after-effects) 0))
                    (simulate (struct-copy game-state state-after-effects
                                           [player-mana (- (game-state-player-mana state-after-effects) 113)]
                                           [mana-spent (+ (game-state-mana-spent state-after-effects) 113)]
                                           [shield-timer 6])
                              #f))
                  (when (and (>= (game-state-player-mana state-after-effects) 173)
                             (= (game-state-poison-timer state-after-effects) 0))
                    (simulate (struct-copy game-state state-after-effects
                                           [player-mana (- (game-state-player-mana state-after-effects) 173)]
                                           [mana-spent (+ (game-state-mana-spent state-after-effects) 173)]
                                           [poison-timer 6])
                              #f))
                  (when (and (>= (game-state-player-mana state-after-effects) 229)
                             (= (game-state-recharge-timer state-after-effects) 0))
                    (simulate (struct-copy game-state state-after-effects
                                           [player-mana (- (game-state-player-mana state-after-effects) 229)]
                                           [mana-spent (+ (game-state-mana-spent state-after-effects) 229)]
                                           [recharge-timer 5])
                              #f)))
                (let* ((damage (game-state-boss-damage state-after-effects))
                       (armor (if (> (game-state-shield-timer state-after-effects) 0) 7 0))
                       (actual-damage (max 1 (- damage armor))))
                  (let ((state-after-attack (struct-copy game-state state-after-effects
                                                        [player-hp (- (game-state-player-hp state-after-effects) actual-damage)])))
                    (simulate state-after-attack #t))))))))))
(module+ main
  (define boss-stats (file->lines "input.txt"))
  (define boss-hp (string->number (cadr (string-split (first boss-stats) ": "))))
  (define boss-damage (string->number (cadr (string-split (second boss-stats) ": "))))
  (define initial-state (game-state 50 500 boss-hp boss-damage 0 0 0 0))
  (simulate initial-state #t)
  (printf "~a\n" min-mana))
