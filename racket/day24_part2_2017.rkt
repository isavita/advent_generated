
#lang racket

(require racket/file)
(require racket/string)
(require racket/list)

(define (read-components filename)
  (with-input-from-file filename
    (lambda ()
      (map (lambda (line)
             (map string->number (string-split (string-trim line) "/")))
           (port->lines)))))

(define (solve components current-port current-strength current-length)
  (let ((best-strength current-strength)
        (best-length current-length))

    (let iterate-components ((idx 0)
                             (current-best-s best-strength)
                             (current-best-l best-length))
       (if (>= idx (length components))
           (values current-best-s current-best-l)

           (let* ((component-at-idx (list-ref components idx))
                  (p-idx (first component-at-idx))
                  (q-idx (second component-at-idx)))

             (if (or (= p-idx current-port) (= q-idx current-port))
                 (let* ((next-port-idx (if (= p-idx current-port) q-idx p-idx))
                        (new-components (append (take components idx) (drop components (+ idx 1))))
                        (new-strength (+ current-strength p-idx q-idx))
                        (new-length (+ current-length 1)))

                   (let-values (((child-strength child-length)
                                 (solve new-components next-port-idx new-strength new-length)))

                     (if (or (> child-length current-best-l)
                             (and (= child-length current-best-l) (> child-strength current-best-s)))
                         (iterate-components (+ idx 1) child-strength child-length)
                         (iterate-components (+ idx 1) current-best-s current-best-l))))

                 (iterate-components (+ idx 1) current-best-s current-best-l)))))))

(define (main args)
  (let* ((filename "input.txt")
         (components (read-components filename))
         (initial-port 0)
         (initial-strength 0)
         (initial-length 0))
    (let-values (((max-strength max-length)
                  (solve components initial-port initial-strength initial-length)))
      (printf "~a\n" max-strength))))

(module+ main
  (main (current-command-line-arguments)))
