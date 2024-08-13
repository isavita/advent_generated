#lang racket

(define (rotate-waypoint waypoint angle)
  (let* ((rad (/ (* angle pi) 180))
         (cos (cos rad))
         (sin (sin rad))
         (x (first waypoint))
         (y (second waypoint)))
    (list (round (+ (* x cos) (* y (- sin))))
          (round (+ (* x sin) (* y cos))))))

(define (process-instructions instructions)
  (define ship (list 0 0))  ; ship's position (east, north)
  (define waypoint (list 10 1))  ; waypoint's relative position (east, north)

  (for ((line instructions))
    (define action (substring line 0 1))
    (define value (string->number (substring line 1)))

    (cond
      [(equal? action "N") (set! waypoint (list (first waypoint) (+ (second waypoint) value)))]
      [(equal? action "S") (set! waypoint (list (first waypoint) (- (second waypoint) value)))]
      [(equal? action "E") (set! waypoint (list (+ (first waypoint) value) (second waypoint)))]
      [(equal? action "W") (set! waypoint (list (- (first waypoint) value) (second waypoint)))]
      [(equal? action "L") (set! waypoint (rotate-waypoint waypoint value))]
      [(equal? action "R") (set! waypoint (rotate-waypoint waypoint (- value)))]
      [(equal? action "F") 
       (set! ship (list (+ (first ship) (* (first waypoint) value))
                        (+ (second ship) (* (second waypoint) value))))]))

  (apply + (map abs ship)))

(define (main)
  (define instructions (map string-trim (file->lines "input.txt")))
  (define distance (process-instructions instructions))
  (printf "Manhattan distance: ~a\n" distance))

(main)