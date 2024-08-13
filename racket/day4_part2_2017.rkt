#lang racket

(define (read-passphrases filename)
  (with-input-from-file filename
    (lambda ()
      (for/list ([line (in-lines)]) 
        (string-split line)))))

(define (valid-passphrase? passphrase)
  (define unique-words (remove-duplicates passphrase))
  (equal? (length unique-words) (length passphrase)))

(define (sorted-string s)
  (apply string (sort (string->list s) char<?)))

(define (valid-passphrase-anagram? passphrase)
  (define sorted-words (map sorted-string passphrase))
  (define unique-sorted (remove-duplicates sorted-words))
  (equal? (length unique-sorted) (length sorted-words)))

(define (count-valid-passphrases passphrases validator)
  (length (filter validator passphrases)))

(define (main)
  (define passphrases (read-passphrases "input.txt"))
  (define valid-count (count-valid-passphrases passphrases valid-passphrase?))
  (define valid-anagram-count (count-valid-passphrases passphrases valid-passphrase-anagram?))
  (printf "Valid passphrases: ~a\n" valid-count)
  (printf "Valid passphrases (no anagrams): ~a\n" valid-anagram-count))

(main)