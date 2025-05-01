
#lang racket

(define (get-cell grid r c grid-height grid-width)
  (if (and (>= r 0) (< r grid-height) (>= c 0) (< c grid-width))
      (vector-ref (vector-ref grid r) c)
      #f))

(define (count-neighbors grid r c grid-height grid-width)
  (let ([on-neighbors 0])
    (for* ([dr (range -1 2)]
           [dc (range -1 2)])
      (unless (and (= dr 0) (= dc 0))
        (let* ([nr (+ r dr)]
               [nc (+ c dc)]
               [neighbor-state (get-cell grid nr nc grid-height grid-width)])
          (when (eq? neighbor-state #\#)
            (set! on-neighbors (add1 on-neighbors))))))
    on-neighbors))

(define (get-next-state grid r c grid-height grid-width)
  (let ([current-state (get-cell grid r c grid-height grid-width)]
        [on-neighbors (count-neighbors grid r c grid-height grid-width)])
    (cond
      [(and (eq? current-state #\#) (not (or (= on-neighbors 2) (= on-neighbors 3))))
       #\.]
      [(and (eq? current-state #\.) (= on-neighbors 3))
       #\#]
      [else
       current-state])))

(define (animate grid)
  (let* ([grid-height (vector-length grid)]
         [grid-width (if (zero? grid-height) 0 (vector-length (vector-ref grid 0)))])
    (build-vector grid-height
                  (lambda (r)
                    (build-vector grid-width
                                  (lambda (c)
                                    (get-next-state grid r c grid-height grid-width)))))))

(define (count-lights-on grid)
  (for*/sum ([row (in-vector grid)] [cell (in-vector row)])
    (if (eq? cell #\#) 1 0)))

(define (main)
  (define initial-list-grid
    (call-with-input-file "input.txt"
      (lambda (p)
        (for/list ([line (port->lines p)])
          (string->list (string-trim line))))))

  (define initial-vector-grid (list->vector (map list->vector initial-list-grid)))

  (let loop ([steps 0]
             [current-grid initial-vector-grid])
    (if (= steps 100)
        (begin
          (display (count-lights-on current-grid))
          (newline))
        (loop (add1 steps) (animate current-grid)))))

(main)
