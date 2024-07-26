
#lang racket

(define (read-input file)
  (with-input-from-file file
    (lambda ()
      (let loop ((lines '()))
        (define line (read-line))
        (if (eof-object? line)
            (reverse lines)
            (loop (cons line lines)))))))

(define (move-head tail dir)
  (match dir
    ["R" (values (+ (car tail) 1) (cadr tail))]
    ["L" (values (- (car tail) 1) (cadr tail))]
    ["U" (values (car tail) (+ (cadr tail) 1))]
    ["D" (values (car tail) (- (cadr tail) 1))]))

(define (update-tail head tail)
  (let* ((dx (- (car head) (car tail)))
         (dy (- (cadr head) (cadr tail)))
         (new-tail (if (or (> (abs dx) 1) (> (abs dy) 1))
                       (list (if (= dx 0) (car tail) (+ (car tail) (if (> dx 0) 1 -1)))
                             (if (= dy 0) (cadr tail) (+ (cadr tail) (if (> dy 0) 1 -1))))
                       tail)))
    new-tail))

(define (count-visited points)
  (define visited (make-hash))
  (for ([p points])
    (hash-set! visited p #t))
  (hash-count visited))

(define (main)
  (define moves (read-input "input.txt"))
  (define head (list 0 0))
  (define tail (list 0 0))
  (define visited '((0 0)))

  (for ([move moves])
    (define parts (string-split move))
    (define dir (first parts))
    (define steps (string->number (second parts)))

    (for ([i (in-range steps)])
      (define-values (new-head-x new-head-y) (move-head head dir))
      (set! head (list new-head-x new-head-y))
      (set! tail (update-tail head tail))
      (set! visited (cons tail visited))))

  (displayln (count-visited visited)))

(main)
