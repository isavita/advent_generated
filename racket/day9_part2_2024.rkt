
#lang racket

(struct file-segment (id start end) #:transparent)

(define (solve)
  ; Read input line
  (define line
    (call-with-input-file "input.txt"
      (lambda (port)
        (read-line port))))

  ; --- Build initial disk layout (using a mutable vector) ---
  (define input-chars (string->list line))
  (define total-size (for/sum ([c input-chars]) (- (char->integer c) (char->integer #\0))))
  (define disk (make-vector total-size #f)) ; Placeholder value #f initially

  (let loop ([chars input-chars]
             [index 0]
             [file-id 0]
             [is-file? #t])
    (unless (null? chars)
      (let* ([len-char (car chars)]
             [len (- (char->integer len-char) (char->integer #\0))]
             [end-index (+ index len)])
        (if is-file?
            (let ([id-str (number->string file-id)])
              (for ([i (in-range index end-index)])
                (vector-set! disk i id-str))
              (loop (cdr chars) end-index (+ file-id 1) #f))
            (begin
              (for ([i (in-range index end-index)])
                (vector-set! disk i "."))
              (loop (cdr chars) end-index file-id #t))))))

  ; --- Build file segments list ---
  (define files
    (reverse ; Accumulator builds in reverse
     (let loop ([i 0]
                [acc '()])
       (if (>= i (vector-length disk))
           acc
           (let ([val (vector-ref disk i)])
             (if (equal? val ".")
                 (loop (+ i 1) acc) ; Skip free space
                 (let* ([id (string->number val)]
                        [start i]
                        ; Find end of this segment
                        [end (let find-end ([j (+ i 1)])
                               (if (or (>= j (vector-length disk))
                                       (not (equal? (vector-ref disk j) val)))
                                   (- j 1)
                                   (find-end (+ j 1))))])
                   (loop (+ end 1) ; Start next iteration after this segment ends
                         (cons (file-segment id start end) acc)))))))))

  ; --- Process files from highest ID to lowest ---
  (define sorted-files (sort files > #:key file-segment-id))

  (for ([fs sorted-files])
    (let* ([file-id (file-segment-id fs)]
           [file-start (file-segment-start fs)]
           [file-end (file-segment-end fs)]
           [file-len (- file-end file-start -1)])

      ; Find leftmost suitable free space before the current file position
      (let find-space ([idx 0]
                       [current-span-start -1]
                       [current-span-len 0])
        (cond
          [(>= idx file-start) (void)] ; Search ended before finding space or reached file start
          [(equal? (vector-ref disk idx) ".")
           (let* ([new-len (+ current-span-len 1)]
                  [new-start (if (= current-span-len 0) idx current-span-start)])
             (if (= new-len file-len)
                 ; --- Found space! Perform the move ---
                 (begin
                   ; Clear old location
                   (for ([i (in-range file-start (+ file-end 1))])
                     (vector-set! disk i "."))
                   ; Copy to new location
                   (let ([id-str (number->string file-id)])
                     (for ([i (in-range new-start (+ new-start file-len))])
                       (vector-set! disk i id-str))))
                 ; --- Continue searching ---
                 (find-space (+ idx 1) new-start new-len)))]
          [else ; Hit a non-free spot, reset span
           (find-space (+ idx 1) -1 0)]))))

  ; --- Calculate checksum ---
  (define checksum
    (for/sum ([i (in-range (vector-length disk))])
      (let ([val (vector-ref disk i)])
        (if (equal? val ".")
            0
            (* i (string->number val))))))

  ; Print the answer
  (displayln checksum))

(define (main)
  (solve))

; Run main when the script is executed
(module+ main
  (main))
