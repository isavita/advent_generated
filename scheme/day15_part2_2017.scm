(define (read-input)
  (with-input-from-file "input.txt"
    (lambda ()
      (list (read) (read)))))

(define (generate-next value factor modulus condition)
  (let loop ((val value))
    (let ((next (modulo (* val factor) modulus)))
      (if (condition next)
          next
          (loop next)))))

(define (count-matches genAStart genBStart)
  (define genAFactor 16807)
  (define genBFactor 48271)
  (define modulus 2147483647)
  (define matches 0)
  (let loop ((genA genAStart) (genB genBStart) (i 0))
    (if (< i 5000000)
        (begin
          (set! genA (generate-next genA genAFactor modulus (lambda (x) (= (modulo x 4) 0))))
          (set! genB (generate-next genB genBFactor modulus (lambda (x) (= (modulo x 8) 0))))
          (if (= (bitwise-and genA 65535) (bitwise-and genB 65535))
              (set! matches (+ matches 1)))
          (loop genA genB (+ i 1)))
        matches)))

(define (main)
  (define inputs (read-input))
  (define genAStart (car inputs))
  (define genBStart (cadr inputs))
  (display (count-matches genAStart genBStart)))

(main)