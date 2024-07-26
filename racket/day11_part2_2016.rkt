
#lang racket

(struct halves (is-chip material))
(struct state (floors elevator-level steps))

(define (main)
  (define input (read-file "input.txt"))
  (define ans (rtg-hell-day (new-initial-state input)))
  (displayln ans))

(define (rtg-hell-day initial-state)
  (define queue (list initial-state))
  (define prev-states (make-hash))
  
  (define (loop)
    (if (null? queue)
        -1
        (let* ((front (first queue))
               (rest (rest queue))
               (hash (state-hash-key front)))
          (if (hash-ref prev-states hash #f)
              (loop rest)
              (begin
                (hash-set! prev-states hash #t)
                (if (state-is-done front)
                    (state-steps front)
                    (loop (append rest (state-get-next-states front)))))))))
  
  (loop))

(define (new-initial-state input)
  (define floors (make-vector 4 '()))
  (for ([line (string-split input "\n")])
    (define parts (map (lambda (v) (string-trim v ",.")) (string-split line " ")))
    (define line-index (vector-length floors))
    (for ([i (in-range (length parts))])
      (define word (list-ref parts i))
      (cond
        [(string=? word "generator")
         (define material (list-ref parts (- i 1)))
         (vector-set! floors line-index (cons (halves #f material) (vector-ref floors line-index)))]
        [(string=? word "microchip")
         (define material (substring (list-ref parts (- i 1)) 0 (string-index (list-ref parts (- i 1)) #\-)))
         (vector-set! floors line-index (cons (halves #t material) (vector-ref floors line-index)))])))
  (state floors 0 0))

(define (state-hash-key s)
  (define gen-chip-pairs '())
  (define map-gen-to-index (make-hash))
  (define map-chip-to-index (make-hash))
  
  (for ([fl-index (in-range (vector-length (state-floors s)))])
    (for ([half (vector-ref (state-floors s) fl-index)])
      (if (halves-is-chip half)
          (hash-set! map-chip-to-index (halves-material half) fl-index)
          (hash-set! map-gen-to-index (halves-material half) fl-index))))
  
  (for ([material (hash-keys map-gen-to-index)])
    (define gen-index (hash-ref map-gen-to-index material))
    (define chip-index (hash-ref map-chip-to-index material))
    (set! gen-chip-pairs (cons (list gen-index chip-index) gen-chip-pairs)))
  
  (set! gen-chip-pairs (sort gen-chip-pairs (lambda (a b) (or (< (first a) (first b)) (and (= (first a) (first b)) (< (second a) (second b)))))))
  (list (state-elevator-level s) gen-chip-pairs))

(define (state-is-valid s)
  (for/fold ([valid #t]) ([i (in-range (vector-length (state-floors s)))])
    (define gens-seen (make-hash))
    (for ([half (vector-ref (state-floors s) i)])
      (if (halves-is-chip half)
          (hash-set! gens-seen (halves-material half) #t)
          (hash-set! gens-seen (halves-material half) #f)))
    (if (hash-count gens-seen)
        (for/or ([half (vector-ref (state-floors s) i)])
          (and (halves-is-chip half) (not (hash-ref gens-seen (halves-material half) #f)))
          )
        valid)))

(define (state-is-done s)
  (define len-sum (for/sum ([fl (in-range 3)]) (vector-length (vector-ref (state-floors s) fl))))
  (= len-sum 0))

(define (state-get-movable-perm-indices s)
  (define current-level (vector-ref (state-floors s) (state-elevator-level s)))
  (define perms-to-move '())
  
  (for ([i (in-range (length current-level))])
    (for ([j (in-range (+ i 1 (length current-level)))])
      (when (< i j)
        (set! perms-to-move (cons (list i j) perms-to-move)))))
  
  (for ([i (in-range (length current-level))])
    (set! perms-to-move (cons (list i) perms-to-move)))
  
  perms-to-move)

(define (state-clone s)
  (state (vector-map (lambda (fl) (copy-list fl)) (state-floors s))
         (state-elevator-level s)
         (state-steps s)))

(define (state-get-next-states s)
  (define future-states '())
  (define movable-perm-indices (state-get-movable-perm-indices s))
  (define ele-diffs (if (< (state-elevator-level s) 3) (list 1) (if (> (state-elevator-level s) 0) (list -1) '())))
  
  (for ([ele-diff ele-diffs])
    (for ([perm-indices movable-perm-indices])
      (define cl (state-clone s))
      (set! (state-elevator-level cl) (+ (state-elevator-level s) ele-diff))
      (set! (state-steps cl) (+ (state-steps s) 1))
      (define old-level (state-elevator-level s))
      (define new-level (state-elevator-level cl))
      
      (for ([index perm-indices])
        (set! (vector-ref (state-floors cl) new-level) (cons (vector-ref (state-floors s) old-level index) (vector-ref (state-floors cl) new-level))))
      (for ([in (in-range (length perm-indices) -1 -1)])
        (set! (vector-ref (state-floors cl) old-level) (remove (vector-ref (state-floors cl) old-level) (vector-ref (state-floors cl) old-level index))))
      )
      (when (state-is-valid cl)
        (set! future-states (cons cl future-states))))))
  
  future-states)

(define (read-file path)
  (define-values (in out) (open-input-file path))
  (define content (read-line in))
  (close-input-port in)
  (string-trim-right content "\n"))

(main)
