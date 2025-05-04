
#lang racket/base
(require racket/file)
(require racket/string)
(require racket/list)
(require json)

(define (compare-state a b)
  (cond
    [(and (integer? a) (integer? b))
     (cond [(< a b) -1] [(> a b) 1] [else 0])]
    [(and (integer? a) (list? b))
     (compare-state (list a) b)]
    [(and (list? a) (integer? b))
     (compare-state a (list b))]
    [(and (list? a) (list? b))
     (let loop ((lst-a a) (lst-b b))
       (cond
         [(and (empty? lst-a) (empty? lst-b))
          0]
         [(empty? lst-a)
          -1]
         [(empty? lst-b)
          1]
         [else
          (let ((cmp-head (compare-state (car lst-a) (car lst-b))))
            (cond
              [(= cmp-head -1) -1]
              [(= cmp-head 1) 1]
              [else (loop (cdr lst-a) (cdr lst-b))]))]))]))

(define (main)
  (define s (file->string "input.txt"))
  (define pairs (string-split s "\n\n"))

  (define packets
    (append-map (lambda (pair-str)
                  (map string->jsexpr (string-split pair-str "\n")))
                pairs))

  (define divider1 (string->jsexpr "[[2]]"))
  (define divider2 (string->jsexpr "[[6]]"))

  (define all-packets (append packets (list divider1 divider2)))

  (define sorted-packets
    (sort all-packets (lambda (a b) (= (compare-state a b) -1))))

  (define pos1 (+ 1 (index-of sorted-packets divider1)))
  (define pos2 (+ 1 (index-of sorted-packets divider2)))

  (print (* pos1 pos2)))

(module+ main
  (main))
