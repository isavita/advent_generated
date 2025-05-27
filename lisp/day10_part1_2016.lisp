
;; Global variables for bots, outputs, and ready bots
(defvar *bots* (make-hash-table :test 'eql)) ; Use eql for integer keys
(defvar *outputs* (make-hash-table :test 'eql))
(defvar *ready-bots* nil) ; A list acting as a stack (LIFO) for bots ready to process

;; Constants for the target chip values we are looking for
(defconstant +target-low-chip+ 17)
(defconstant +target-high-chip+ 61)

;; Variable to store the ID of the bot that compares the target chips
(defvar *found-comparison-bot-id* nil)

;; Define a structure for a bot to hold its state and instructions
(defstruct (bot (:conc-name bot-))
  (chips '()) ; List of chips currently held by the bot
  low-target-type ; Keyword: :BOT or :OUTPUT
  low-target-id   ; Integer ID
  high-target-type ; Keyword: :BOT or :OUTPUT
  high-target-id) ; Integer ID

;; Function to get a bot object by ID, creating it if it doesn't exist
(defun get-or-create-bot (id)
  (or (gethash id *bots*)
      (setf (gethash id *bots*) (make-bot))))

;; Function to distribute a chip to a target (bot or output)
(defun give-chip (target-type target-id chip)
  (case target-type
    (:bot
     (let* ((bot (get-or-create-bot target-id)))
       ;; Add the chip to the bot's collection
       (setf (bot-chips bot) (cons chip (bot-chips bot)))
       ;; If the bot now has two chips, add its ID to the ready queue
       (when (= (length (bot-chips bot)) 2)
         (push target-id *ready-bots*)))) ; Push to the front (stack-like)
    (:output
     ;; Add the chip to the output bin's collection
     (push chip (gethash target-id *outputs*)))))

;; Function to process a bot's actions (when it has two chips)
(defun process-bot (bot-id)
  (let* ((bot (get-or-create-bot bot-id))
         ;; Get the two chips and sort them to identify low and high
         (chips (sort (bot-chips bot) #'<)) ; Sort in ascending order
         (low-chip (first chips))
         (high-chip (second chips)))

    ;; Check if this bot is the one comparing the target chips
    (when (and (= low-chip +target-low-chip+)
               (= high-chip +target-high-chip+))
      (setf *found-comparison-bot-id* bot-id)) ; Store the bot ID

    ;; Clear the chips from the current bot as it's distributing them
    (setf (bot-chips bot) '())

    ;; Give the low chip to its designated target
    (give-chip (bot-low-target-type bot) (bot-low-target-id bot) low-chip)
    ;; Give the high chip to its designated target
    (give-chip (bot-high-target-type bot) (bot-high-target-id bot) high-chip)))

;; Main simulation loop
(defun run-simulation ()
  ;; Continue as long as there are bots ready to act
  (loop while *ready-bots*
        do (process-bot (pop *ready-bots*)))) ; Pop from the front (stack-like)

;; Function to parse a line of input and update game state
(defun parse-instruction (line)
  (let ((parsed-line (read-from-string line)))
    (case (first parsed-line)
      ;; Handle "value X goes to bot Y" instructions
      (VALUE
       (let ((value (second parsed-line))
             (bot-id (fifth parsed-line)))
         (give-chip :bot bot-id value)))
      ;; Handle "bot X gives low to TYPE Y and high to TYPE Z" instructions
      (BOT
       (let* ((bot-id (second parsed-line))
              (low-target-type-sym (eighth parsed-line)) ; e.g., BOT or OUTPUT
              (low-target-id (ninth parsed-line))
              (high-target-type-sym (nth 13 parsed-line)) ; e.g., BOT or OUTPUT
              (high-target-id (nth 14 parsed-line))
              (bot (get-or-create-bot bot-id)))
         ;; Set the bot's rules for chip distribution
         (setf (bot-low-target-type bot) (intern (string-upcase low-target-type-sym) "KEYWORD"))
         (setf (bot-low-target-id bot) low-target-id)
         (setf (bot-high-target-type bot) (intern (string-upcase high-target-type-sym) "KEYWORD"))
         (setf (bot-high-target-id bot) high-target-id)))))))

;; Main entry point for the program
(defun main ()
  ;; Reset global state for a clean run, useful if `main` is called multiple times
  (clrhash *bots*)
  (clrhash *outputs*)
  (setf *ready-bots* nil)
  (setf *found-comparison-bot-id* nil)

  ;; Read and parse instructions from input.txt
  (with-open-file (stream "input.txt" :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          do (parse-instruction line)))

  ;; Run the simulation until no more bots can act
  (run-simulation)

  ;; Print the result for Part 1 to standard output
  (when *found-comparison-bot-id*
    (format t "Bot responsible for comparing ~a and ~a: ~a~%"
            +target-low-chip+ +target-high-chip+ *found-comparison-bot-id*))

  ;; Optional: For Part 2, you might print values from specific output bins.
  ;; Example:
  ;; (let ((val0 (first (gethash 0 *outputs*)))
  ;;       (val1 (first (gethash 1 *outputs*)))
  ;;       (val2 (first (gethash 2 *outputs*))))
  ;;   (when (and val0 val1 val2)
  ;;     (format t "Product of chips in output 0, 1, and 2: ~a~%" (* val0 val1 val2))))
  )

;; To run this program, save it as a .lisp file (e.g., `balance-bots.lisp`),
;; ensure `input.txt` is in the same directory, then load the file in a Lisp
;; environment (like SBCL, CCL, etc.) and call `(main)`.
;; Example in SBCL:
;; (load "balance-bots.lisp")
;; (main)
