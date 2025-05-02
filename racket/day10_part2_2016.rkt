
#lang racket

(struct instruction (low-target-type low-target-id high-target-type high-target-id) #:transparent)

; Parses a target string like "bot 5" or "output 0"
(define (parse-target str)
  (match (string-split str)
    [(list target-type id-str)
     (values (string->symbol target-type) (string->number id-str))]))

; Parses a single line of input and updates the state maps
(define (parse-line line bots instructions initial-values)
  (cond
    [(regexp-match #px"^value (\\d+) goes to bot (\\d+)" line)
     => (lambda (m)
          (let ([value (string->number (list-ref m 1))]
                [bot-id (string->number (list-ref m 2))])
            ; Store initial values separately first
            (hash-update! initial-values bot-id (lambda (chips) (cons value chips)) '())))]
    [(regexp-match #px"^bot (\\d+) gives low to (\\w+ \\d+) and high to (\\w+ \\d+)" line)
     => (lambda (m)
          (let ([bot-id (string->number (list-ref m 1))]
                [low-target-str (list-ref m 2)]
                [high-target-str (list-ref m 3)])
            (let-values ([(low-type low-id) (parse-target low-target-str)]
                         [(high-type high-id) (parse-target high-target-str)])
              (hash-set! instructions bot-id (instruction low-type low-id high-type high-id)))))]
    [else (error "Invalid input line:" line)]))

; Gives a chip to a target (bot or output) and updates the ready queue if a bot becomes ready
; Returns the updated ready-queue
(define (give-chip value target-type target-id bots outputs ready-queue)
  (cond
    [(eq? target-type 'bot)
     ; Add chip to the target bot
     (hash-update! bots target-id (lambda (chips) (cons value chips)) '())
     ; If the target bot now has 2 chips, add it to the ready queue
     (if (= (length (hash-ref bots target-id)) 2)
         (cons target-id ready-queue) ; Return new queue with bot added
         ready-queue)] ; Return original queue
    [(eq? target-type 'output)
     ; Add chip to the target output bin
     (hash-update! outputs target-id (lambda (chips) (cons value chips)) '())
     ready-queue] ; Return original queue (outputs don't become ready)
    [else (error "Unknown target type:" target-type)]))

; Main simulation function
(define (run-simulation bots outputs instructions initial-ready-queue)
  (let loop ([current-ready initial-ready-queue]
             [part1-found #f])
    (if (empty? current-ready)
        ; Base case: No more bots are ready, return the result found
        part1-found
        ; Recursive step: Process one ready bot
        (let* ([bot-id (first current-ready)] ; Get the next ready bot
               [remaining-ready (rest current-ready)]) ; Queue for this iteration

          ; Check if the bot still has exactly 2 chips (might have changed)
          (if (not (= (length (hash-ref bots bot-id '())) 2))
              ; Bot no longer has 2 chips (maybe processed already or state changed unexpectedly)
              (loop remaining-ready part1-found)
              ; Bot has 2 chips, proceed
              (let* ([chips (hash-ref bots bot-id)]
                     ; Bot gives away its chips
                     [_ (hash-set! bots bot-id '())]
                     ; Sort chips to find low and high
                     [sorted-chips (sort chips <)]
                     [low-val (first sorted-chips)]
                     [high-val (second sorted-chips)]
                     ; Get the instruction for this bot
                     [instr (hash-ref instructions bot-id)])

                ; --- Part 1 Check ---
                (define new-part1-found
                  (if (and (= low-val 17) (= high-val 61))
                      bot-id ; Found the target bot
                      part1-found)) ; Keep previous result or #f

                ; --- Give chips away ---
                ; Give low chip and potentially update the ready queue
                (define ready-after-low
                  (give-chip low-val
                             (instruction-low-target-type instr)
                             (instruction-low-target-id instr)
                             bots outputs remaining-ready))

                ; Give high chip and potentially update the ready queue further
                (define final-ready
                  (give-chip high-val
                             (instruction-high-target-type instr)
                             (instruction-high-target-id instr)
                             bots outputs ready-after-low))

                ; Continue the loop with the updated ready queue and part1 result
                (loop final-ready new-part1-found)))))))

; Main entry point
(define (main)
  ; Initialize state using mutable hash tables
  (define bots (make-hash))          ; bot-id -> list of chip values
  (define outputs (make-hash))       ; output-id -> list of chip values
  (define instructions (make-hash)) ; bot-id -> instruction struct
  (define initial-values (make-hash)) ; Temporary storage for value instructions

  ; --- 1. Parse Input ---
  (with-input-from-file "input.txt"
    (lambda ()
      (for ([line (in-lines)])
        (parse-line line bots instructions initial-values))))

  ; --- 2. Distribute Initial Values & Find Initially Ready Bots ---
  (define initial-ready-queue
    (for/list ([(bot-id chips) (in-hash initial-values)]
               #:when (= (length chips) 2)) ; Add only if bot starts with 2 chips
      ; Set initial state for bots receiving values
      (hash-set! bots bot-id chips)
      bot-id))

  ; Also need to add bots from initial_values that didn't start with 2 chips
   (for ([(bot-id chips) (in-hash initial-values)])
      (unless (hash-has-key? bots bot-id) ; Add if not already added (i.e. didn't have 2 chips)
          (hash-set! bots bot-id chips)))

  ; --- 3. Run Simulation ---
  (define part1-answer (run-simulation bots outputs instructions initial-ready-queue))

  ; --- 4. Output Results ---
  (displayln (~a "Part 1: " part1-answer))

  ; Calculate Part 2 result
  (let ([out0 (first (hash-ref outputs 0 '(0)))] ; Default to 0 if output bin is empty
        [out1 (first (hash-ref outputs 1 '(0)))]
        [out2 (first (hash-ref outputs 2 '(0)))])
     ; Check if any output bin was unexpectedly empty, though problem implies they won't be.
     (when (or (zero? out0) (zero? out1) (zero? out2))
       (eprintf "Warning: Output bin 0, 1, or 2 might be empty or contain 0.\n"))
    (displayln (~a "Part 2: " (* out0 out1 out2)))))


; --- Run the main function ---
(main)
