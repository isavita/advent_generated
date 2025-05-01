
#lang racket

(define (rotate pattern)
  (let* ([parts (string-split pattern "/")]
         [size (length parts)])
    (string-join
     (for/list ([x (in-range size)])
       (list->string
        (for/list ([y (in-range (sub1 size) -1 -1)])
          (string-ref (list-ref parts y) x))))
     "/")))

(define (flip pattern)
  (let ([parts (string-split pattern "/")])
    (string-join (map (lambda (s) (list->string (reverse (string->list s)))) parts) "/")))

(define (get-transformations pattern)
  (let loop ([p pattern] [transforms '()] [rotated-count 0])
    (if (= rotated-count 4)
        transforms
        (let ([next-p (rotate p)])
          (loop next-p (cons p transforms) (add1 rotated-count))))))

(define (parse-rules file-path)
  (define rules (make-hash))
  (with-input-from-file file-path
    (lambda ()
      (for ([line (in-lines)])
        (let* ([parts (string-split line " => ")]
               [input-pattern (list-ref parts 0)]
               [output-pattern (list-ref parts 1)]
               [transforms (get-transformations input-pattern)]
               [flipped-transforms (get-transformations (flip input-pattern))])
          (for ([p (append transforms flipped-transforms)])
            (hash-set! rules p output-pattern))))))
  rules)

(define (enhance pattern rules)
  (hash-ref rules pattern ""))

(define (transform-grid grid rules)
  (let* ([grid-size (length grid)]
         [sub-size (if (even? grid-size) 2 3)]
         [num-blocks (quotient grid-size sub-size)]
         [new-sub-size (add1 sub-size)]
         [new-grid-size (* num-blocks new-sub-size)])

    (for/list ([r (in-range new-grid-size)])
      (let* ([y-block (quotient r new-sub-size)]
             [dy (remainder r new-sub-size)])

        (let ([row-parts '()])
          (for ([x-block (in-range num-blocks)])
            (let* ([y-start (* y-block sub-size)]
                   [x-start (* x-block sub-size)]
                   [square-pattern (string-join
                                    (for/list ([sy (in-range sub-size)])
                                      (substring (list-ref grid (+ y-start sy)) x-start (+ x-start sub-size)))
                                    "/")]
                   [new-square-parts (string-split (enhance square-pattern rules) "/")]
                   [row-part (list-ref new-square-parts dy)])
              (set! row-parts (cons row-part row-parts))))
          (string-join (reverse row-parts) ""))))))

(define (count-pixels grid)
  (let ([count 0])
    (for ([row grid])
      (for ([c (in-string row)])
        (when (eq? c #\#)
          (set! count (add1 count)))))
    count))

(define (main)
  (let ([rules (parse-rules "input.txt")])
    (let loop ([grid '(".#." "..#" "###")] [iteration 0])
      (if (= iteration 5)
          (printf "~a\n" (count-pixels grid))
          (loop (transform-grid grid rules) (add1 iteration))))))

(main)
