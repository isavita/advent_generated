#lang racket

(define (read-decks filename)
  (define player1 '())
  (define player2 '())
  (define current-deck 'player1)
  (for ([line (in-lines (open-input-file filename))])
    (cond
      [(string=? line "") (set! current-deck 'player2)]
      [(string-contains? line "Player") #t]
      [else
       (define card (string->number line))
       (if (eq? current-deck 'player1)
           (set! player1 (cons card player1))
           (set! player2 (cons card player2)))]))
  (values (reverse player1) (reverse player2)))

(define (play-game deck1 deck2)
  (cond
    [(null? deck1) deck2]
    [(null? deck2) deck1]
    [else
     (define card1 (car deck1))
     (define card2 (car deck2))
     (define new-deck1 (cdr deck1))
     (define new-deck2 (cdr deck2))
     (if (> card1 card2)
         (play-game (append new-deck1 (list card1 card2)) new-deck2)
         (play-game new-deck1 (append new-deck2 (list card2 card1))))]))

(define (calculate-score winning-deck)
  (for/sum ([i (in-range (length winning-deck))])
    (* (list-ref winning-deck i) (- (length winning-deck) i))))

(define (main)
  (define-values (player1Deck player2Deck) (read-decks "input.txt"))
  (define winningDeck (play-game player1Deck player2Deck))
  (printf "~a\n" (calculate-score winningDeck)))

(main)