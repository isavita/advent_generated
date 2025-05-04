
#lang racket

(require racket/file)

(define (all-zeros? nums)
  (andmap zero? nums))

(define (diffs nums)
  (if (< (length nums) 2)
      '()
      (map - (rest nums) (drop-right nums 1))))

(define (generate-series seq)
  (if (all-zeros? seq)
      (list seq)
      (let ((next-seq (diffs seq)))
        (cons seq (generate-series next-seq)))))

(define (predict-past series)
  (foldr (lambda (seq acc) (- (first seq) acc))
         0
         series))

(define (main args)
  (let* ((filename "input.txt")
         (lines (file->lines filename))
         (histories (map (lambda (line)
                           (map string->number (string-split line)))
                         lines))
         (total-prediction (apply + (map (lambda (history)
                                            (let ((series (generate-series history)))
                                              (predict-past series)))
                                          histories))))
    (print total-prediction)))

(module+ main
  (main (current-command-line-arguments)))
