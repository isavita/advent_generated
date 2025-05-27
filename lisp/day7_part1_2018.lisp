
(defun main ()
  "Reads step dependencies from 'input.txt', determines the correct
   order of completion based on dependencies and alphabetical priority,
   and prints the result to standard output."

  ;; Hash tables to store the graph, in-degrees, and all unique steps.
  ;; :test 'eql is suitable for characters.
  (let ((graph (make-hash-table :test 'eql))      ; Maps step (char) to list of steps it enables
        (in-degree (make-hash-table :test 'eql))  ; Maps step (char) to its number of prerequisites
        (all-steps (make-hash-table :test 'eql))) ; Set of all unique step characters encountered

    ;; 1. Parse input file to build graph and in-degrees
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil) ; Read line by line until EOF
            while line
            do (let ((pre-char (char line 5))     ; Character at index 5 is the prerequisite step
                     (post-char (char line 36)))   ; Character at index 36 is the dependent step

                 ;; Add post-char to the list of steps enabled by pre-char.
                 ;; (gethash pre-char graph nil) retrieves the current list of dependents for pre-char,
                 ;; defaulting to NIL if pre-char is not yet a key. PUSH adds post-char to this list.
                 (push post-char (gethash pre-char graph nil))

                 ;; Increment the in-degree for post-char.
                 ;; (gethash post-char in-degree 0) retrieves the current in-degree, defaulting to 0.
                 ;; INCF then increments this value.
                 (incf (gethash post-char in-degree 0))

                 ;; Add both characters to our set of all unique steps encountered.
                 ;; Setting value to T indicates presence in the set.
                 (setf (gethash pre-char all-steps t) t)
                 (setf (gethash post-char all-steps t) t))))

    ;; 2. Initialize the priority queue (available-steps) and result string
    (let ((available-steps '()) ; List to hold steps ready for processing, kept sorted
          ;; Result string pre-allocated to maximum possible length (A-Z = 26 steps).
          ;; The actual number of steps might be less, so we track the index.
          (result-string (make-string (hash-table-count all-steps) :initial-element #\Space))
          (result-idx 0)) ; Current index in result-string to place the next character

      ;; Populate initial available steps: those with an in-degree of 0.
      (maphash (lambda (step-char dummy-value)
                 (declare (ignore dummy-value)) ; We only care about the key (step-char)
                 ;; If the step's in-degree is 0 (or it's not in the map, which defaults to 0),
                 ;; it's available to start.
                 (when (zerop (gethash step-char in-degree 0))
                   (push step-char available-steps)))
               all-steps)
      
      ;; Sort the initial available steps alphabetically.
      (setf available-steps (sort available-steps #'char<))

      ;; 3. Process steps until no more are available
      (loop while available-steps
            do (let* ((current-step (pop available-steps))) ; Get and remove the first (alphabetically smallest) step
                 
                 ;; Add the current step to our result string.
                 (setf (char result-string result-idx) current-step)
                 (incf result-idx)

                 ;; For each step that CURRENT-STEP enables:
                 (dolist (next-step (gethash current-step graph nil))
                   ;; Decrement its prerequisite count.
                   (decf (gethash next-step in-degree))
                   
                   ;; If its prerequisite count becomes zero, it's now available.
                   (when (zerop (gethash next-step in-degree))
                     (push next-step available-steps))) ; Add to available list

                 ;; Re-sort the available-steps to maintain alphabetical priority for the next iteration.
                 ;; This is crucial for selecting the correct next step.
                 (setf available-steps (sort available-steps #'char<))))

      ;; 4. Print the final ordered sequence.
      ;; SUBSEQ is used to get only the used portion of result-string.
      (format t "~A~%" (subseq result-string 0 result-idx)))))

;; Call the main function to run the program when the Lisp file is loaded or executed.
(main)
