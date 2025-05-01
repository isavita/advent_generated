
#lang racket

(define (make-pos r c) (cons r c))
(define (pos-row p) (car p))
(define (pos-col p) (cdr p))

(define dirs
  '((-1 . -1) (-1 . 0) (-1 . 1) (0 . 1) (1 . 1) (1 . 0) (1 . -1) (0 . -1))) ; NW, N, NE, E, SE, S, SW, W

(define order '(1 5 7 3)) ; Indices for N, S, W, E in dirs

(define (add-vec pos vec)
  (make-pos (+ (pos-row pos) (car vec)) (+ (pos-col pos) (cdr vec))))

(define (pos-in-map? pos positions)
  (hash-has-key? positions pos))

(define (around-any-elf? pos positions)
  (for/or ([d dirs])
    (pos-in-map? (add-vec pos d) positions)))

(define (dir-neighbors pos dir-idx)
  (list (add-vec pos (list-ref dirs (modulo (- dir-idx 1) 8)))
        (add-vec pos (list-ref dirs dir-idx))
        (add-vec pos (list-ref dirs (modulo (+ dir-idx 1) 8)))))

(define (dir-empty? pos dir-idx positions)
  (and (not (pos-in-map? (add-vec pos (list-ref dirs (modulo (- dir-idx 1) 8))) positions))
       (not (pos-in-map? (add-vec pos (list-ref dirs dir-idx)) positions))
       (not (pos-in-map? (add-vec pos (list-ref dirs (modulo (+ dir-idx 1) 8))) positions))))

(define (parse-input filename)
  (with-input-from-file filename
    (lambda ()
      (let ([positions (make-hash)])
        (for ([line (in-lines)] [r (in-naturals)])
          (for ([char (in-string line)] [c (in-naturals)])
            (when (char=? char #\#)
              (hash-set! positions (make-pos r c) #t))))
        positions))))

(define (run-round positions direction-order-idx)
  (define proposals (make-hash)) ; dest -> list of sources
  (define someone-moved? #f)

  ;; Phase 1: Collect proposals
  (for ([(pos _) (in-hash positions)])
    (when (around-any-elf? pos positions)
      (let loop ([i 0])
        (when (< i 4)
          (let* ([dir-idx (list-ref order (modulo (+ direction-order-idx i) 4))]
                 [dest (add-vec pos (list-ref dirs dir-idx))])
            (if (dir-empty? pos dir-idx positions)
                ;; Propose move
                (begin
                  (hash-set! proposals dest (cons pos (hash-ref proposals dest '())))
                  #f) ; Stop checking directions for this elf
                ;; Check next direction
                (loop (+ i 1))))))))

  ;; Phase 2: Execute moves
  (define next-positions (make-hash (hash->list positions))) ; Copy current positions
  (for ([(dest sources) (in-hash proposals)])
    (when (= (length sources) 1)
      (define src (car sources))
      (hash-remove! next-positions src)
      (hash-set! next-positions dest #t)
      (set! someone-moved? #t)))

  (values next-positions someone-moved?))

(define (main)
  (let ([positions (parse-input "input.txt")])
    (let loop ([round-num 1] [direction-order-idx 0] [current-positions positions])
      (let-values ([(next-positions moved?) (run-round current-positions direction-order-idx)])
        (if moved?
            (loop (+ round-num 1) (modulo (+ direction-order-idx 1) 4) next-positions)
            (printf "~a\n" round-num))))))

(module+ main
  (main))
