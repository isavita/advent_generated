#lang racket

(define (read-passports filename)
  (define input (file->string filename))
  (define lines (string-split input "\n\n" #:trim? #t))
  (map (lambda (p) (string-replace p "\n" " ")) lines))

(define (valid-passport? passport required-fields)
  (andmap (lambda (field) (string-contains? passport (format "~a:" field))) required-fields))

(define (count-valid-passports passports required-fields)
  (length (filter (lambda (p) (valid-passport? p required-fields)) passports)))

(define (main)
  (define passports (read-passports "input.txt"))
  (define required-fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))
  (printf "~a\n" (count-valid-passports passports required-fields)))

(main)