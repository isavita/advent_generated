
#lang racket
(require racket/file racket/string racket/set)

(define (run-program instructions-vec swap-idx)
  (let loop ([idx 0] [acc 0] [visited (set)])
    (cond
      [(>= idx (vector-length instructions-vec)) (values acc #t)]
      [(set-member? visited idx) (values acc #f)]
      [else
       (let* ([instr (vector-ref instructions-vec idx)]
              [parts (string-split instr)]
              [op (list-ref parts 0)]
              [arg (string->number (list-ref parts 1))])
         (let ([current-op (if (and (= idx swap-idx) (string=? op "jmp")) "nop"
                            (if (and (= idx swap-idx) (string=? op "nop")) "jmp"
                                op))]
               [current-arg arg])
           (cond
             [(string=? current-op "acc") (loop (+ idx 1) (+ acc current-arg) (set-add visited idx))]
             [(string=? current-op "jmp") (loop (+ idx current-arg) acc (set-add visited idx))]
             [(string=? current-op "nop") (loop (+ idx 1) acc (set-add visited idx))])))])))

(define (fix-program instructions-vec)
  (let ([n (vector-length instructions-vec)])
    (let loop ([i 0])
      (if (< i n)
          (let* ([instr (vector-ref instructions-vec i)]
                 [parts (string-split instr)]
                 [op (list-ref parts 0)])
            (if (or (string=? op "jmp") (string=? op "nop"))
                (let-values ([(acc terminated) (run-program instructions-vec i)])
                  (if terminated
                      acc
                      (loop (+ i 1))))
                (loop (+ i 1))))
          (error "No fix found")))))

(define (main)
  (let ([instructions-vec (list->vector (file->lines "input.txt"))])
    (println (fix-program instructions-vec))))

(main)
