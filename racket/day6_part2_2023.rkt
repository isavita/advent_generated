
#lang racket

(define (parse-line line)
  (let* ([parts (string-split line ":")]
         [num-str (second parts)]
         [cleaned-str (string-replace num-str " " "")])
    (string->number cleaned-str)))

(define (winning? hold-time time distance)
  (> (* hold-time (- time hold-time)) distance))

(define (find-first-winning-time time distance low high)
  (let loop ([low low] [high high] [ans (+ high 1)])
    (if (> low high)
        ans
        (let ([mid (floor (/ (+ low high) 2))])
          (if (winning? mid time distance)
              (loop low (- mid 1) mid)
              (loop (+ mid 1) high ans))))))

(define (find-last-winning-time time distance low high)
  (let loop ([low low] [high high] [ans (- low 1)])
    (if (> low high)
        ans
        (let ([mid (floor (/ (+ low high) 2))])
          (if (winning? mid time distance)
              (loop (+ mid 1) high mid)
              (loop low (- mid 1) ans))))))

(define (main)
  (let* ([lines (file->lines "input.txt")]
         [time (parse-line (first lines))]
         [distance (parse-line (second lines))])
    (let* ([h-start (find-first-winning-time time distance 1 (- time 1))]
           [h-end (find-last-winning-time time distance 1 (- time 1))])
      (if (> h-start h-end)
          (printf "~a\n" 0)
          (printf "~a\n" (- h-end h-start -1))))))

(main)
