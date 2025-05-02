
#lang racket

(struct item (is-chip material) #:transparent)
(struct state (floors elevator-level steps) #:transparent)

(define (string->items s)
  (define parts (map (lambda (w) (string-trim w '(#\, #\.))) (string-split s " ")))
  (let loop ([i 0] [items '()])
    (cond
      [(>= i (length parts)) (reverse items)]
      [(= (list-ref parts i) "generator")
       (let ([material (list-ref parts (- i 1))])
         (loop (+ i 1) (cons (item #f material) items)))]
      [(= (list-ref parts i) "microchip")
       (let ([material (substring (list-ref parts (- i 1)) 0 (string-find (list-ref parts (- i 1)) "-comp"))])
         (loop (+ i 1) (cons (item #t material) items)))]
      [else (loop (+ i 1) items)])))

(define (new-initial-state input-str)
  (let ([lines (string-split input-str "\n")])
    (state
     (list (string->items (list-ref lines 0))
           (string->items (list-ref lines 1))
           (string->items (list-ref lines 2))
           (string->items (list-ref lines 3)))
     0
     0)))

(define (valid-floor? floor)
  (let ([gens '()] [chips '()])
    (for ([it floor])
      (if (item-is-chip it)
          (set! chips (cons (item-material it) chips))
          (set! gens (cons (item-material it) gens))))
    (if (empty? gens)
        #t ; No generators, chips are fine
        (andmap (lambda (chip-mat) (or (member chip-mat gens) (not (empty? (filter (lambda (gen-mat) (not (equal? gen-mat chip-mat))) gens))))) chips))))

(define (is-valid? s)
  (andmap valid-floor? (state-floors s)))

(define (is-done? s)
  (andmap empty? (take (state-floors s) 3)))

(define (hash-key s)
  (let ([gen-map (make-hash)] [chip-map (make-hash)])
    (for ([fl-idx (in-range 4)] [floor (state-floors s)])
      (for ([it floor])
        (if (item-is-chip it)
            (hash-set! chip-map (item-material it) fl-idx)
            (hash-set! gen-map (item-material it) fl-idx))))
    (let ([pairs (sort (map (lambda (mat) (list (hash-ref gen-map mat) (hash-ref chip-map mat))) (hash-keys gen-map))
                       (lambda (p1 p2)
                         (or (< (car p1) (car p2))
                             (and (= (car p1) (car p2)) (< (cadr p1) (cadr p2))))))])
      (string-append (number->string (state-elevator-level s)) (format "~a" pairs)))))

(define (clone-state s)
  (state (map (lambda (fl) (append '() fl)) (state-floors s))
         (state-elevator-level s)
         (state-steps s)))

(define (get-movable-perm-indices floor)
  (let ([len (length floor)])
    (append
     (for*/list ([i (in-range len)] [j (in-range (+ i 1) len)]) (list i j))
     (for/list ([i (in-range len)]) (list i)))))

(define (get-next-states s)
  (let* ([current-level (state-elevator-level s)]
         [current-floor (list-ref (state-floors s) current-level)]
         [movable-perms (get-movable-perm-indices current-floor)]
         [ele-diffs (append (if (< current-level 3) '(1) '()) (if (> current-level 0) '(-1) '()))])
    (let loop ([diffs ele-diffs] [next-states '()])
      (if (empty? diffs)
          (reverse next-states)
          (let ([ele-diff (car diffs)]
                [rest-diffs (cdr diffs)])
            (let inner-loop ([perms movable-perms] [current-diff-states '()])
              (if (empty? perms)
                  (loop rest-diffs (append (reverse current-diff-states) next-states))
                  (let* ([perm-indices (car perms)]
                         [rest-perms (cdr perms)]
                         [cl (clone-state s)]
                         [new-level (+ current-level ele-diff)]
                         [items-to-move (map (lambda (idx) (list-ref current-floor idx)) perm-indices)])

                    ; Remove items from old floor and add to new floor
                    (define new-current-floor
                      (filter (lambda (it) (not (member it items-to-move))) current-floor))
                    (define new-new-floor
                      (append (list-ref (state-floors cl) new-level) items-to-move))

                    (define new-floors
                      (list-set (list-set (state-floors cl) current-level new-current-floor) new-level new-new-floor))

                    (set! cl (state new-floors new-level (+ (state-steps cl) 1)))

                    (if (is-valid? cl)
                        (inner-loop rest-perms (cons cl current-diff-states))
                        (inner-loop rest-perms current-diff-states))))))))))

(define (bfs initial-state)
  (let ([queue (list initial-state)]
        [visited (make-hash)])
    (let loop ([q queue] [visited-set visited])
      (when (not (empty? q))
        (let* ([current-state (car q)]
               [rest-q (cdr q)])
          (if (is-done? current-state)
              (state-steps current-state)
              (let ([h-key (hash-key current-state)])
                (if (hash-has-key? visited-set h-key)
                    (loop rest-q visited-set) ; State already visited
                    (begin
                      (hash-set! visited-set h-key #t)
                      (let ([next-states (get-next-states current-state)])
                        (loop (append rest-q next-states) visited-set))))))))))) ; Append for BFS order


(define (rtg-hell-day input-str part)
  (let ([initial-state (new-initial-state input-str)])
    (when (= part 2)
      (let ([f0 (list-ref (state-floors initial-state) 0)])
        (set! initial-state
              (state
               (list (append f0 (list (item #f "elerium") (item #t "elerium") (item #f "dilithium") (item #t "dilithium")))
                     (list-ref (state-floors initial-state) 1)
                     (list-ref (state-floors initial-state) 2)
                     (list-ref (state-floors initial-state) 3))
               (state-elevator-level initial-state)
               (state-steps initial-state)))))
    (bfs initial-state)))


(module+ main
  (define input-data (file->string "input.txt"))
  (define part 1) ; Set to 2 for part 2
  (define ans (rtg-hell-day input-data part))
  (print ans))
