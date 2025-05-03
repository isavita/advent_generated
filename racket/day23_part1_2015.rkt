
#lang racket

(require racket/match) ; For cleaner pattern matching on instructions

; --- Data Structures ---
; Registers: Represented by a mutable hash mapping symbols 'a and 'b to integers.
; Instruction: Parsed into a list, e.g., '(inc a), '(jmp +5), '(jie b -2)
; Program: A vector of instructions for efficient random access using the instruction pointer.

; --- Parsing ---

; Parses a register string ("a" or "b") into a symbol ('a or 'b).
(define (parse-register reg-str)
  (string->symbol reg-str))

; Parses an offset string ("+N" or "-N") into an integer N or -N.
(define (parse-offset offset-str)
  ; string->number correctly handles "+" or "-" prefixes.
  (string->number offset-str))

; Parses a single line of assembly code into an instruction list.
; Handles different instruction formats (1 or 2 arguments after opcode).
(define (parse-instruction line)
  (match (string-split line #px"[, ]+") ; Split by comma or space(s), filter empty
    [`(,opcode ,reg-str) #:when (member opcode '("hlf" "tpl" "inc"))
     (list (string->symbol opcode) (parse-register reg-str))]
    [`(,opcode ,offset-str) #:when (equal? opcode "jmp")
     (list (string->symbol opcode) (parse-offset offset-str))]
    [`(,opcode ,reg-str ,offset-str) #:when (member opcode '("jie" "jio"))
     (list (string->symbol opcode) (parse-register reg-str) (parse-offset offset-str))]
    [other (error 'parse-instruction "Invalid instruction format: ~a in line: ~s" other line)]))

; Reads the program from the specified file and returns it as a vector of instructions.
(define (read-program filename)
  (with-input-from-file filename
    (lambda ()
      (list->vector
       (for/list ([line (in-lines (current-input-port))])
         (parse-instruction line))))))

; --- Simulation Engine ---

; Runs the program (vector of instructions) starting with given initial register values.
; Returns the final value of register 'b.
(define (run-program instructions initial-a initial-b)
  (let ([regs (make-hash `((a . ,initial-a) (b . ,initial-b)))] ; Mutable register state
        [ip 0]                                                  ; Instruction pointer
        [prog-len (vector-length instructions)])
    ; Loop until the instruction pointer is out of bounds
    (let loop ([current-ip ip])
      (if (or (< current-ip 0) (>= current-ip prog-len))
          ; Program terminated, return the final value of register 'b
          (hash-ref regs 'b)
          ; Fetch and execute the current instruction
          (let ([instr (vector-ref instructions current-ip)])
            (match instr
              ; hlf r: Halves register r, moves to next instruction. Uses integer division.
              [`(hlf ,r)
               (hash-set! regs r (quotient (hash-ref regs r) 2))
               (loop (+ current-ip 1))]
              ; tpl r: Triples register r, moves to next instruction.
              [`(tpl ,r)
               (hash-set! regs r (* (hash-ref regs r) 3))
               (loop (+ current-ip 1))]
              ; inc r: Increments register r, moves to next instruction.
              [`(inc ,r)
               (hash-set! regs r (+ (hash-ref regs r) 1))
               (loop (+ current-ip 1))]
              ; jmp offset: Jumps by the offset relative to the current instruction.
              [`(jmp ,offset)
               (loop (+ current-ip offset))]
              ; jie r, offset: Jumps if register r is even.
              [`(jie ,r ,offset)
               (if (even? (hash-ref regs r))
                   (loop (+ current-ip offset))
                   (loop (+ current-ip 1)))]
              ; jio r, offset: Jumps if register r is exactly 1.
              [`(jio ,r ,offset)
               (if (= (hash-ref regs r) 1)
                   (loop (+ current-ip offset))
                   (loop (+ current-ip 1)))]
              ; Should not happen with valid input and parsing
              [_ (error 'run-program "Execution error: Unknown instruction format: ~a" instr)]))))))

; --- Main Entry Point ---

(define (main)
  ; Read the program from the input file
  (define program (read-program "input.txt"))
  ; Run the simulation starting with registers a=0, b=0
  (define final-b (run-program program 0 0))
  ; Print the final value of register b to standard output
  (printf "~a\n" final-b))

; Execute the main function when the script is run
(main)
