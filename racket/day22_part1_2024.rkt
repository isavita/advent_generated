
#lang racket

; --- Core Secret Number Generation Logic ---

; Prunes the secret number by taking its value modulo 16777216 (2^24)
(define prune (lambda (n) (modulo n 16777216)))

; Mixes a value into the secret number using bitwise XOR
(define mix (lambda (secret value) (bitwise-xor secret value)))

; Generates the next secret number based on the current one
; Implements the three steps:
; 1. Multiply by 64, mix, prune
; 2. Integer divide by 32, mix, prune
; 3. Multiply by 2048, mix, prune
(define (generate-next-secret current-secret)
  (let* (;; Step 1: Multiply by 64, mix, prune
         (s1 (prune (mix current-secret (* current-secret 64))))
         ;; Step 2: Integer divide by 32, mix, prune
         (s2 (prune (mix s1 (quotient s1 32))))
         ;; Step 3: Multiply by 2048, mix, prune
         (s3 (prune (mix s2 (* s2 2048)))))
    ;; The result of the third prune is the next secret
    s3))

; Generates the Nth new secret number starting from an initial secret
; This is done by applying generate-next-secret N times
(define (generate-nth-secret initial-secret num-steps)
  (let loop ((current-secret initial-secret)
             (steps-done 0))
    (if (= steps-done num-steps)
        current-secret ; Return the secret after num-steps generations
        (loop (generate-next-secret current-secret) (+ steps-done 1)))))

; --- Input/Output and Main Logic ---

; Reads initial secret numbers from the specified file
(define (read-initial-secrets filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '()))
        (let ((line (read-line)))
          (if (eof-object? line)
              (reverse (map string->number lines)) ; Convert lines to numbers and reverse
              (loop (cons line lines))))))))

; Main function to solve the challenge
; Reads initial secrets, generates the 2000th secret for each, and sums them
(define (solve filename num-steps)
  (let ((initial-secrets (read-initial-secrets filename)))
    (apply + (map (lambda (initial-secret)
                    (generate-nth-secret initial-secret num-steps))
                  initial-secrets))))

; --- Program Entry Point ---

; Define the input filename and the number of steps to simulate
(define input-filename "input.txt")
(define simulation-steps 2000)

; Execute the solver and print the total sum
(let ((total-sum (solve input-filename simulation-steps)))
  (printf "~a~n" total-sum))
