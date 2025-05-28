
;; Standard library utilities often available in competitive programming setups or defined locally.
;; For a full runnable script, these are necessary.

(defun split-string (string delimiter &key (remove-empty-p t))
  "Splits a string by a delimiter character.
   If remove-empty-p is true, empty strings resulting from splitting are removed.
   Example: (split-string \"a,b,,c\" #\\,) => (\"a\" \"b\" \"c\")"
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        for sub = (subseq string i (or j (length string)))
        when (or (not remove-empty-p) (not (string= sub "")))
        collect sub
        while j))

(defun trim-whitespace (s)
  "Removes leading and trailing whitespace from a string.
   Whitespace characters defined by char-whitespace-p."
  (string-trim '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page) s))

(defun string-reverse (s)
  "Returns a new string with the characters of S in reverse order."
  (let ((n (length s))
        (new-s (make-string (length s))))
    (loop for i from 0 below n
          do (setf (char new-s i) (char s (- (1- n) i))))
    new-s))

;; Trie structure definition
(defstruct trie-node
  (children (make-hash-table :test 'eql)) ; Maps char to trie-node
  (is-pattern-end nil)) ; Boolean: t if this node marks the end of a valid pattern

(defun build-trie (patterns)
  "Builds a trie from a list of patterns.
   For this problem, patterns are expected to be REVERSED versions of the actual towel patterns."
  (let ((root (make-trie-node)))
    (dolist (pattern patterns)
      (let ((curr root))
        (loop for char across pattern
              do (let ((next-node (gethash char (trie-node-children curr))))
                   (unless next-node
                     (setq next-node (make-trie-node))
                     (setf (gethash char (trie-node-children curr)) next-node))
                   (setq curr next-node)))
        (setf (trie-node-is-pattern-end curr) t)))
    root))

(defun count-ways (design reversed-trie-root)
  "Calculates the number of ways a design string can be formed using towel patterns.
   This dynamic programming approach uses a Trie built from REVERSED patterns
   to efficiently check for valid patterns that form suffixes of the current design prefix.
   Complexity: O(L * M) where L is design length and M is max pattern length."
  (let* ((n (length design))
         (dp (make-array (1+ n) :element-type 'integer :initial-element 0)))
    (setf (aref dp 0) 1) ; Base case: 1 way to form an empty string

    (loop for i from 1 to n ; 'i' is the current ending position (exclusive) in the design string
          do (let ((curr-node reversed-trie-root))
               ;; Iterate backwards from design[i-1] down to design[0]
               ;; This traverses the design string characters in reverse order,
               ;; allowing matching against the reversed patterns in the trie.
               (loop for j from (1- i) downto 0 ; 'j' is the starting char index for a potential pattern (original order)
                     for char = (char design j) ; Current character from design to match in the trie
                     do (let ((next-node (gethash char (trie-node-children curr-node))))
                          (unless next-node
                            (return)) ; Character not found in trie from current node, no more reversed patterns can match
                          (setq curr-node next-node)
                          (when (trie-node-is-pattern-end curr-node)
                            ;; If the sequence of characters design[j...i-1] (read backwards: design[i-1]...design[j])
                            ;; forms a valid reversed pattern, then design[j...i-1] (original order) is a valid towel pattern.
                            ;; Add the number of ways to form the prefix design[0...j-1] (stored in dp[j]) to dp[i].
                            (incf (aref dp i) (aref dp j)))))))
    (aref dp n)))

(defun read-puzzle-input (filepath)
  "Reads the towel patterns and desired designs from the specified file.
   Returns two values: (list of patterns) and (list of designs)."
  (with-open-file (in filepath :direction :input)
    (let* ((patterns-line (read-line in nil nil))
           (patterns (mapcar #'trim-whitespace
                             (split-string patterns-line #\, :remove-empty-p t)))
           (designs nil)
           (line nil))
      ;; Read lines until a blank line is encountered (signifying end of patterns section)
      (loop (setf line (read-line in nil nil))
            (unless line (return)) ; End of file
            (when (string= (trim-whitespace line) "") (return)) ; Blank line found
            )
      ;; Read remaining lines as designs
      (loop (setf line (read-line in nil nil))
            (unless line (return))
            (let ((trimmed-line (trim-whitespace line)))
              (unless (string= trimmed-line "") ; Skip any lingering empty lines
                (push trimmed-line designs))))
      (values patterns (nreverse designs))))) ; Nreverse to restore original order from push

(defun main ()
  "Main entry point for the Linen Layout puzzle solver.
   Reads input, computes answers for Part 1 and Part 2, and prints results."

  ;; Optimize for speed (common in competitive programming)
  (declaim (optimize (speed 3) (safety 0) (debug 0)))

  (multiple-value-bind (patterns designs)
      (read-puzzle-input "input.txt")

    ;; For the optimized DP, we build a trie on reversed patterns once.
    (let ((reversed-patterns (mapcar #'string-reverse patterns))
          (reversed-trie-root nil))

      ;; Build the trie
      (setf reversed-trie-root (build-trie reversed-patterns))

      ;; Part 1: How many designs are possible?
      (let ((possible-designs-count 0))
        (dolist (design designs)
          (when (> (count-ways design reversed-trie-root) 0)
            (incf possible-designs-count)))
        (format t "Part 1: Number of possible designs: ~a~%" possible-designs-count))

      ;; Part 2: What is the total number of different ways to make each design?
      (let ((total-ways 0))
        (dolist (design designs)
          (incf total-ways (count-ways design reversed-trie-root)))
        (format t "Part 2: Total number of ways: ~a~%" total-ways)))))

;; Execute the main function when the script is loaded/run.
(main)
