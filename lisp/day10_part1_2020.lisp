
(defun main ()
  (let* ((adapters '())
         (diff-1s 0)
         (diff-3s 0))
    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil)
            while line
            do (push (parse-integer line) adapters)))
    (setf adapters (sort adapters #'<))
    (setf adapters (cons 0 adapters))
    (setf adapters (append adapters (list (+ (car (last adapters)) 3))))
    (loop for prev = (car adapters) then curr
          for curr in (cdr adapters)
          do (let ((diff (- curr prev)))
               (cond ((= diff 1) (incf diff-1s))
                     ((= diff 3) (incf diff-3s)))))
    (print (* diff-1s diff-3s))))

(main)
