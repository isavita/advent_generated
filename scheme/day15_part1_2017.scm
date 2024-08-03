(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (list (read) (read)))))

(define (generate-next gen factor modulus)
  (modulo (* gen factor) modulus))

(define (count-matches gen-a-start gen-b-start)
  (define gen-a gen-a-start)
  (define gen-b gen-b-start)
  (define matches 0)
  (define gen-a-factor 16807)
  (define gen-b-factor 48271)
  (define modulus 2147483647)

  (do ((i 0 (+ i 1)))
      ((= i 40000000) matches)
    (set! gen-a (generate-next gen-a gen-a-factor modulus))
    (set! gen-b (generate-next gen-b gen-b-factor modulus))
    (when (= (bitwise-and gen-a 65535) (bitwise-and gen-b 65535))
      (set! matches (+ matches 1)))))

(define (main)
  (let* ((input (read-input "input.txt"))
         (gen-a-start (car input))
         (gen-b-start (cadr input)))
    (display (count-matches gen-a-start gen-b-start))))

(main)