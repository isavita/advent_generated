
#lang racket/base

(require racket/hash racket/file racket/string)

(define (parse-rule line)
  (define parts (string-split line " bags contain "))
  (define bag-color (list-ref parts 0))
  (define contents-string (list-ref parts 1))
  (if (string=? contents-string "no other bags.")
      (cons bag-color '())
      (let ([bag-specs (string-split contents-string ", ")])
        (define contained-colors
          (map (lambda (bag-spec)
                 (define tokens (string-split bag-spec " "))
                 (string-join (list (list-ref tokens 1) (list-ref tokens 2)) " "))
               bag-specs))
        (cons bag-color contained-colors))))

(define (build-rules lines)
  (make-hash (map parse-rule lines)))

(define (can-reach-shiny-gold? bag-color rules-ht memo-ht)
  (cond
    [(hash-has-key? memo-ht bag-color) (hash-ref memo-ht bag-color)]
    [(string=? bag-color "shiny gold") (hash-set! memo-ht bag-color #t) #t]
    [else
     (define contained-colors (hash-ref rules-ht bag-color '()))
     (define result
       (ormap (lambda (c-color)
                (can-reach-shiny-gold? c-color rules-ht memo-ht))
              contained-colors))
     (hash-set! memo-ht bag-color result)
     result]))

(module+ main
  (define input-string (file->string "input.txt"))
  (define lines (string-split (string-trim input-string) "\n"))
  (define rules-ht (build-rules lines))
  (define memo-ht (make-hash))
  (define count 0)
  (for ([bag-color (hash-keys rules-ht)])
    (when (can-reach-shiny-gold? bag-color rules-ht memo-ht)
      (set! count (add1 count))))
  (print (- count 1)))
