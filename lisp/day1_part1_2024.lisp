
(defun main ()
  "Reads two lists of numbers from 'input.txt', sorts them,
   and prints the total absolute difference between corresponding elements."

  (let ((left-list '())
        (right-list '()))

    ;; Open the input file for reading.
    ;; `with-open-file` ensures the file is closed automatically.
    (with-open-file (in "input.txt" :direction :input)
      ;; Loop until the end of the file is reached.
      (loop
        ;; READ reads the next Lisp object from the stream.
        ;; It skips whitespace and correctly parses integers.
        ;; `nil :eof` as arguments means if EOF is reached, return :eof
        ;; instead of signalling an error.
        (let ((num1 (read in nil :eof))
              (num2 (read in nil :eof)))
          ;; If the first number read is :eof, we've reached the end of the file.
          (when (eq num1 :eof)
            (return)) ; Exit the loop

          ;; Add the numbers to their respective lists.
          ;; PUSH adds elements to the front, so the lists will be reversed.
          ;; This is fine since we sort them later.
          (push num1 left-list)
          (push num2 right-list))))

    ;; Sort both lists in ascending order.
    ;; SORT modifies the list in place, which is efficient here.
    (setf left-list (sort left-list #'<))
    (setf right-list (sort right-list #'<))

    (let ((total-distance 0))
      ;; Iterate through both sorted lists simultaneously.
      ;; `LOOP FOR ... IN ... FOR ... IN ...` is concise for parallel iteration.
      (loop for l-val in left-list
            for r-val in right-list
            do
               ;; Calculate the absolute difference between the paired numbers
               ;; and add it to the total-distance.
               (incf total-distance (abs (- l-val r-val))))

      ;; Print the final total distance to standard output.
      (format t "~a~%" total-distance)))
)

;; To run the program from the command line (e.g., using SBCL):
;; sbcl --script your-program.lisp
;;
;; Or from a REPL:
;; (main)
(main)
