
#lang racket

(module+ main
  (define (main)
    (define data (file->string "input.txt"))
    (define passports (string-split data "\n\n"))

    (define required-fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

    (define (valid-passport? passport-str)
      (define fields-list (regexp-split #px"\\s+" passport-str))
      (define fields-ht (make-hash))
      (for ([field-str fields-list])
        (when (string-contains? field-str ":")
          (define parts (string-split field-str ":"))
          (when (= (length parts) 2)
            (hash-set! fields-ht (first parts) (second parts)))))

      (if (andmap (lambda (field) (hash-has-key? fields-ht field)) required-fields)
          (let ([byr (hash-ref fields-ht "byr")]
                [iyr (hash-ref fields-ht "iyr")]
                [eyr (hash-ref fields-ht "eyr")]
                [hgt (hash-ref fields-ht "hgt")]
                [hcl (hash-ref fields-ht "hcl")]
                [ecl (hash-ref fields-ht "ecl")]
                [pid (hash-ref fields-ht "pid")])
            (and
             (let ([n (string->number byr)]) (and n (>= n 1920) (<= n 2002)))
             (let ([n (string->number iyr)]) (and n (>= n 2010) (<= n 2020)))
             (let ([n (string->number eyr)]) (and n (>= n 2020) (<= n 2030)))
             (cond
               [(string-suffix? hgt "cm")
                (let ([n (string->number (substring hgt 0 (- (string-length hgt) 2)))])
                  (and n (>= n 150) (<= n 193)))]
               [(string-suffix? hgt "in")
                (let ([n (string->number (substring hgt 0 (- (string-length hgt) 2)))])
                  (and n (>= n 59) (<= n 76)))]
               [else #f])
             (regexp-match? #px"^#[0-9a-f]{6}$" hcl)
             (member ecl '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))
             (regexp-match? #px"^[0-9]{9}$" pid)))
          #f))

    (print (for/sum ([passport passports])
             (if (valid-passport? passport) 1 0))))

  (main))
