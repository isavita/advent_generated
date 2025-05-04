
#lang racket

(define (fmxv xmn)
  (if (<= xmn 0)
      0
      (let loop ((v 0))
        (if (>= (* v (+ v 1)) (* 2 xmn))
            v
            (loop (+ v 1))))))

(define (sim ixv iyv xmn xmx ymn ymx)
  (let loop ((x 0) (y 0) (cxv ixv) (cyv iyv))
    (let ((nx (+ x cxv))
          (ny (+ y cyv)))
      (if (and (>= nx xmn) (<= nx xmx)
               (>= ny ymn) (<= ny ymx))
          #t
          (if (or (and (< nx xmn) (<= cxv 0))
                  (and (> nx xmx) (>= cxv 0))
                  (and (< ny ymn) (< cyv 0)))
              #f
              (let ((ncxv (if (> cxv 0) (- cxv 1) (if (< cxv 0) (+ cxv 1) 0)))
                    (ncyv (- cyv 1)))
                (loop nx ny ncxv ncyv)))))))

(module+ main
  (define xmn 0)
  (define xmx 0)
  (define ymn 0)
  (define ymx 0)

  (with-input-from-file "input.txt"
    (lambda ()
      (let* ((l (read-line))
             (p (string-split l ", "))
             (xp (substring (list-ref p 0) 15))
             (yp (substring (list-ref p 1) 2))
             (xr (string-split xp ".."))
             (yr (string-split yp "..")))
        (set! xmn (string->number (list-ref xr 0)))
        (set! xmx (string->number (list-ref xr 1)))
        (set! ymn (string->number (list-ref yr 0)))
        (set! ymx (string->number (list-ref yr 1))))))

  (define mixv (fmxv xmn))
  (define maxv xmx)
  (define miyv ymn)
  (define mayv (if (>= ymn 0) ymx (- (- ymn) 1)))

  (define h (make-hash))

  (for* ((xv (in-range mixv (add1 maxv)))
         (yv (in-range miyv (add1 mayv))))
    (when (sim xv yv xmn xmx ymn ymx)
      (hash-set! h (string-append (number->string xv) "," (number->string yv)) #t)))

  (print (hash-count h)))
