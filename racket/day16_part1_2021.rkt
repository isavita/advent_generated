
#lang racket

(define (read-bits bin-str start count)
  (string->number (substring bin-str start (+ start count)) 2))

(define (hex->bin hex-str)
  (define hex-map
    '((#\0 . "0000") (#\1 . "0001") (#\2 . "0010") (#\3 . "0011")
      (#\4 . "0100") (#\5 . "0101") (#\6 . "0110") (#\7 . "0111")
      (#\8 . "1000") (#\9 . "1001") (#\A . "1010") (#\B . "1011")
      (#\C . "1100") (#\D . "1101") (#\E . "1110") (#\F . "1111")))
  (apply string-append
         (map (lambda (char) (cdr (assoc char hex-map)))
              (string->list hex-str))))

(define (parse-packet bin-str start-idx)
  (define version (read-bits bin-str start-idx 3))
  (define type-id (read-bits bin-str (+ start-idx 3) 3))
  (define current-idx (+ start-idx 6))

  (define total-version-sum version)

  (if (= type-id 4)
      (let loop ((idx current-idx))
        (if (char=? (string-ref bin-str idx) #\1)
            (loop (+ idx 5))
            (values total-version-sum (+ idx 5))))
      (let ((length-type-id (read-bits bin-str current-idx 1)))
        (let ((op-idx (+ current-idx 1)))
          (if (= length-type-id 0)
              (let* ((sub-packet-length (read-bits bin-str op-idx 15))
                     (content-idx (+ op-idx 15)))
                (let loop ((idx content-idx)
                           (versions-sum total-version-sum))
                  (if (>= idx (+ content-idx sub-packet-length))
                      (values versions-sum idx)
                      (call-with-values (lambda () (parse-packet bin-str idx))
                        (lambda (sub-version-sum next-idx)
                          (loop next-idx (+ versions-sum sub-version-sum)))))))
              (let* ((num-sub-packets (read-bits bin-str op-idx 11))
                     (content-idx (+ op-idx 11)))
                (let loop ((idx content-idx)
                           (packets-parsed 0)
                           (versions-sum total-version-sum))
                  (if (= packets-parsed num-sub-packets)
                      (values versions-sum idx)
                      (call-with-values (lambda () (parse-packet bin-str idx))
                        (lambda (sub-version-sum next-idx)
                          (loop next-idx
                                (+ packets-parsed 1)
                                (+ versions-sum sub-version-sum))))))))))))

(define (main)
  (define hex-str (with-input-from-file "input.txt" read-line))
  (define bin-str (hex->bin hex-str))
  (call-with-values (lambda () (parse-packet bin-str 0))
    (lambda (version-sum _)
      (printf "~a\n" version-sum))))

(main)
