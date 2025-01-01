
#lang racket

(define (parse-levels line)
  (map string->number (string-split line)))

(define (safe-report? levels)
  (cond
    [(< (length levels) 2) #f]
    [else
     (let* ([first-diff (- (list-ref levels 1) (list-ref levels 0))]
            [increasing? (> first-diff 0)])
       (let loop ([i 0] [prev (list-ref levels 0)])
         (cond
           [(= i (- (length levels) 1)) #t]
           [else
            (let* ([curr (list-ref levels (+ i 1))]
                   [diff (- curr prev)])
              (cond
                [(= diff 0) #f]
                [(or (and increasing? (<= diff 0))
                     (and (not increasing?) (>= diff 0))) #f]
                [(let ([abs-diff (abs diff)])
                   (or (< abs-diff 1) (> abs-diff 3))) #f]
                [else (loop (+ i 1) curr)]))])))]))

(define (safe-with-one-removal? levels)
  (for/or ([i (in-range (length levels))])
    (safe-report? (append (take levels i) (drop levels (+ i 1))))))

(define (solve)
  (let ([safe-report-count 0])
    (with-input-from-file "input.txt"
      (lambda ()
        (for ([line (in-lines)])
          (let ([levels (parse-levels line)])
            (when (or (safe-report? levels) (safe-with-one-removal? levels))
              (set! safe-report-count (+ safe-report-count 1)))))))
    safe-report-count))

(printf "~a\n" (solve))
