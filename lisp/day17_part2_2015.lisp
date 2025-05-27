
(defpackage :advent-of-code-2015-day-17
  (:use :cl)
  (:export :main))

(in-package :advent-of-code-2015-day-17)

;; Global constant for the target eggnog amount
(defconstant +target-eggnog+ 150
  "The total amount of eggnog to store in liters for the challenge.")

;; Helper function to check if a hash table is empty
(defun hash-table-empty-p (ht)
  "Returns T if the hash table HT is empty, NIL otherwise."
  (zerop (hash-table-count ht)))

;; Function to read container capacities from the input file
(defun read-capacities (filename)
  "Reads integer capacities, one per line, from the specified FILENAME.
   Returns a list of integers."
  (with-open-file (stream filename :direction :input
                            :if-does-not-exist :error)
    (loop for line = (read-line stream nil nil)
          while line
          collect (parse-integer line))))

;; Main function to solve both parts of the problem
(defun main ()
  "Main entry point of the program. Reads input from 'input.txt',
   solves both parts of the puzzle, and prints the results to standard output."
  (let* ((capacities (read-capacities "input.txt"))
         (target +target-eggnog+))

    ;; --- Part 1: Count total combinations ---
    ;; dp-part1[j] stores the number of ways to sum to j.
    ;; The array is initialized to 0. dp-part1[0] is set to 1
    ;; because there's one way to make a sum of 0 (by using no containers).
    (let ((dp-part1 (make-array (1+ target) :initial-element 0)))
      (setf (aref dp-part1 0) 1)

      ;; Iterate through each container capacity.
      ;; For each capacity 'c', update the dp array from TARGET down to 'c'.
      ;; This ensures that each container is considered only once for a given sum 'j',
      ;; treating distinct containers with the same capacity as unique.
      (dolist (c capacities)
        (loop for j from target downto c
              do (incf (aref dp-part1 j) (aref dp-part1 (- j c)))))

      (format t "Part 1: Number of combinations: ~a~%" (aref dp-part1 target)))

    ;; --- Part 2: Find minimum containers and count ways for that minimum ---
    ;; dp-part2[j] stores a hash table, where keys are the number of containers
    ;; used to reach sum 'j', and values are the number of ways to achieve that sum
    ;; with that specific number of containers.
    (let ((dp-part2 (make-array (1+ target))))
      ;; Initialize each element of dp-part2 with an empty hash table.
      (dotimes (i (1+ target))
        (setf (aref dp-part2 i) (make-hash-table :test 'eql))) ; :test 'eql is good for integer keys
      
      ;; Base case: 0 liters can be made with 0 containers in 1 way.
      (setf (gethash 0 (aref dp-part2 0)) 1)

      ;; Iterate through each container capacity, similar to Part 1.
      (dolist (c capacities)
        (loop for j from target downto c
              do (let ((ways-to-prev-sum (aref dp-part2 (- j c))))
                   ;; If there are ways to make (j - c), process them.
                   ;; maphash does nothing on an empty hash table, so the unless is optional.
                   (unless (hash-table-empty-p ways-to-prev-sum)
                     (maphash (lambda (prev-num-containers prev-ways)
                                (let* ((new-num-containers (1+ prev-num-containers))
                                       (current-ways-for-this-count (gethash new-num-containers (aref dp-part2 j) 0)))
                                  ;; Accumulate the number of ways for the new container count.
                                  (setf (gethash new-num-containers (aref dp-part2 j))
                                        (+ current-ways-for-this-count prev-ways))))
                              ways-to-prev-sum)))))
      
      ;; After processing all containers, analyze the results for the target sum.
      (let ((final-combinations (aref dp-part2 target)))
        (if (hash-table-empty-p final-combinations)
            (format t "Part 2: No combinations found for the target sum.~%")
            (let ((min-containers nil)
                  (ways-for-min-containers 0))
              
              ;; Find the minimum number of containers used among all combinations for 'target'.
              ;; And also retrieve the corresponding number of ways.
              (maphash (lambda (num-containers ways)
                         (when (or (null min-containers) (< num-containers min-containers))
                           (setf min-containers num-containers)
                           (setf ways-for-min-containers ways)))
                       final-combinations)
              
              (format t "Part 2: Minimum containers: ~a, Ways to achieve it: ~a~%"
                      min-containers
                      ways-for-min-containers)))))
  (values)) ; Indicate no useful return value from main

;; Entry point for the script
;; This ensures that the `main` function is called when the file is loaded
;; or executed, providing a clean execution flow for a script.
(eval-when (:execute :load-toplevel)
  (main))
