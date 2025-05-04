
#lang racket

(require racket/string)
(require racket/match)
(require racket/hash)

(define (read-input filename)
  (define lines (file->lines filename))
  (let loop ((ls lines) (grid-lines '()) (moves '()))
    (if (empty? ls)
        (values (reverse grid-lines) (string-append* (reverse moves)))
        (let ((line (car ls)))
          (if (string-contains? line "#")
              (loop (cdr ls) (cons line grid-lines) '())
              (loop (cdr ls) grid-lines (cons line moves)))))))

(define (parse-grid grid-lines)
  (define rows (length grid-lines))
  (define cols (string-length (car grid-lines)))
  (define grid (make-vector rows))
  (for ([r (in-range rows)])
    (let* ((line (list-ref grid-lines r))
           (chars (string->list line)))
      (define row-vec (make-vector cols))
      (for ([c (in-range cols)])
        (vector-set! row-vec c (list-ref chars c)))
      (vector-set! grid r row-vec)))
  grid)

(define (find-robot grid)
  (define rows (vector-length grid))
  (define cols (vector-length (vector-ref grid 0)))
  (let loop ((r 0))
    (if (= r rows)
        (error "Robot not found")
        (let loop-col ((c 0))
          (if (= c cols)
              (loop (+ r 1))
              (if (eq? (vector-ref (vector-ref grid r) c) #\@)
                  (list r c)
                  (loop-col (+ c 1))))))))

(define dirs (hash '#\^ (list -1 0)
                   '#\v (list 1 0)
                   '#\< (list 0 -1)
                   '#\> (list 0 1)))

(define (get-cell grid r c)
  (vector-ref (vector-ref grid r) c))

(define (set-cell! grid r c val)
  (vector-set! (vector-ref grid r) c val))

(define (push-boxes grid r c dr dc)
  (define nr (+ r dr))
  (define nc (+ c dc))
  (define rows (vector-length grid))
  (define cols (vector-length (vector-ref grid 0)))
  (if (or (< nr 0) (>= nr rows) (< nc 0) (>= nc cols))
      #f
      (let ((next-char (get-cell grid nr nc)))
        (case next-char
          [(#\#) #f]
          [(#\O)
           (if (push-boxes grid nr nc dr dc)
               (begin
                 (set-cell! grid nr nc #\O)
                 (set-cell! grid r c #\.)
                 #t)
               #f)]
          [(#\.)
           (set-cell! grid nr nc #\O)
           (set-cell! grid r c #\.)
           #t]
          [else (error (format "Unexpected character during push: ~a" next-char))]))))

(define (simulate-move grid robot-r robot-c move-char)
  (define dir (hash-ref dirs move-char))
  (define dr (first dir))
  (define dc (second dir))
  (define nr (+ robot-r dr))
  (define nc (+ robot-c dc))
  (define rows (vector-length grid))
  (define cols (vector-length (vector-ref grid 0)))
  (if (or (< nr 0) (>= nr rows) (< nc 0) (>= nc cols))
      (list robot-r robot-c)
      (let ((next-char (get-cell grid nr nc)))
        (case next-char
          [(#\#)
           (list robot-r robot-c)]
          [(#\O)
           (if (push-boxes grid nr nc dr dc)
               (begin
                 (set-cell! grid nr nc #\@)
                 (set-cell! grid robot-r robot-c #\.)
                 (list nr nc))
               (list robot-r robot-c))]
          [(#\.)
           (set-cell! grid nr nc #\@)
           (set-cell! grid robot-r robot-c #\.)
           (list nr nc)]
          [else (error (format "Unexpected character at robot target: ~a" next-char))]))))

(define (calculate-score grid)
  (define rows (vector-length grid))
  (define cols (vector-length (vector-ref grid 0)))
  (let loop ((r 0) (total-sum 0))
    (if (= r rows)
        total-sum
        (let loop-col ((c 0) (row-sum 0))
          (if (= c cols)
              (loop (+ r 1) (+ total-sum row-sum))
              (let ((cell (get-cell grid r c)))
                (if (eq? cell #\O)
                    (loop-col (+ c 1) (+ row-sum (* r 100) c))
                    (loop-col (+ c 1) row-sum))))))))

(module+ main
  (define-values (grid-lines moves) (read-input "input.txt"))
  (define grid (parse-grid grid-lines))
  (define robot-pos (find-robot grid))

  (let loop ((remaining-moves (string->list moves))
             (current-robot-pos robot-pos))
    (if (empty? remaining-moves)
        (void)
        (let* ((move (car remaining-moves))
               (new-robot-pos (simulate-move grid (first current-robot-pos) (second current-robot-pos) move)))
          (loop (cdr remaining-moves) new-robot-pos))))

  (define final-score (calculate-score grid))
  (printf "~a\n" final-score))
