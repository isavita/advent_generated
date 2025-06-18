
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROBOT-EXPLORER.
       AUTHOR. AI Programmer.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'input.txt'
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01  INPUT-RECORD PIC X(1024).

       WORKING-STORAGE SECTION.
       *> Constants
       01  MEM-SIZE         PIC 9(5) VALUE 8192.
       01  MAP-DIM          PIC 9(3) VALUE 101.
       01  MAP-OFFSET       PIC 9(3) VALUE 50.
       01  MAX-QUEUE        PIC 9(7) VALUE 20402. *> MAP-DIM * MAP-DIM * 2

       01  UNKNOWN          PIC S9(1) VALUE -1.
       01  WALL             PIC S9(1) VALUE 0.
       01  FLOOR            PIC S9(1) VALUE 1.
       01  OXYGEN           PIC S9(1) VALUE 2.

       01  RUN-OK           PIC 9(1) VALUE 0.
       01  RUN-INPUT-NEEDED PIC 9(1) VALUE 1.
       01  RUN-OUTPUT-READY PIC 9(1) VALUE 2.
       01  RUN-HALTED       PIC 9(1) VALUE 3.

       01  NORTH            PIC 9(1) VALUE 1.
       01  SOUTH            PIC 9(1) VALUE 2.
       01  WEST             PIC 9(1) VALUE 3.
       01  EAST             PIC 9(1) VALUE 4.

       *> Intcode Computer State
       01  INTCODE-STATE.
           05  MEM              OCCURS 8192 TIMES PIC S9(18) COMP-5.
           05  IP               PIC S9(18) COMP-5.
           05  RELATIVE-BASE    PIC S9(18) COMP-5.
           05  HALTED           PIC 9(1) COMP-5.
           05  INPUT-VAL        PIC S9(18) COMP-5.
           05  OUTPUT-VAL       PIC S9(18) COMP-5.

       *> BFS Queue State
       01  BFS-STATE.
           05  X                PIC S9(3) COMP-5.
           05  Y                PIC S9(3) COMP-5.
           05  DIST             PIC S9(9) COMP-5.
           05  INTCODE-STATE-COPY  PIC X(1024). *> Store as string for copy

       *> BFS Queue
       01  BFS-QUEUE.
           05  ITEMS            OCCURS 20402 TIMES.
               10  QUEUE-ITEM   PIC X(1024). *> Store BFS-STATE as string
           05  HEAD             PIC 9(5) COMP-5.
           05  TAIL             PIC 9(5) COMP-5.
           05  COUNT            PIC 9(5) COMP-5.

       *> Global Variables
       01  MAP              OCCURS 101 TIMES INDEXED BY Y-IDX.
           05  MAP-ROW        OCCURS 101 TIMES PIC S9(1) COMP-5.

       01  MIN-DIST         OCCURS 101 TIMES INDEXED BY Y-IDX.
           05  MIN-DIST-ROW   OCCURS 101 TIMES PIC S9(9) COMP-5.

       01  OXYGEN-X         PIC S9(3) COMP-5 VALUE -1.
       01  OXYGEN-Y         PIC S9(3) COMP-5 VALUE -1.
       01  FINAL-DISTANCE   PIC S9(9) COMP-5 VALUE -1.

       01  WS-FILE-STATUS   PIC XX.
       01  WS-PROGRAM-DATA  PIC X(1024).
       01  WS-CURRENT-POS   PIC 9(5).
       01  WS-CHAR          PIC X.
       01  WS-NUM-STR       PIC X(20).
       01  WS-NUM-VAL       PIC S9(18) COMP-5.
       01  WS-INDEX         PIC 9(5) COMP-5.
       01  WS-OPCODE        PIC 9(2) COMP-5.
       01  WS-MODE1         PIC 9(1) COMP-5.
       01  WS-MODE2         PIC 9(1) COMP-5.
       01  WS-MODE3         PIC 9(1) COMP-5.
       01  WS-PARAM1        PIC S9(18) COMP-5.
       01  WS-PARAM2        PIC S9(18) COMP-5.
       01  WS-PARAM3        PIC S9(18) COMP-5.
       01  WS-ADDR          PIC S9(18) COMP-5.
       01  WS-MOVE-CMD      PIC 9(1) COMP-5.
       01  WS-NEXT-X        PIC S9(3) COMP-5.
       01  WS-NEXT-Y        PIC S9(3) COMP-5.
       01  WS-NEXT-DIST     PIC S9(9) COMP-5.
       01  WS-STATUS-CODE   PIC S9(1) COMP-5.
       01  WS-INTCODE-STATE PIC X(1024). *> Temporary for state copy

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-SYSTEM.
           PERFORM EXPLORE.
           STOP RUN.

       INITIALIZE-SYSTEM.
           MOVE SPACES TO WS-PROGRAM-DATA.
           OPEN INPUT INPUT-FILE.
           IF WS-FILE-STATUS NOT = '00' THEN
               DISPLAY 'Error opening input.txt: ' WS-FILE-STATUS
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF.

           READ INPUT-FILE RECORD INTO WS-PROGRAM-DATA.
           CLOSE INPUT-FILE.

           MOVE SPACES TO WS-NUM-STR.
           MOVE 1 TO WS-CURRENT-POS.
           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > LENGTH(WS-PROGRAM-DATA)
               MOVE WS-PROGRAM-DATA(WS-CURRENT-POS:1) TO WS-CHAR
               IF WS-CHAR = ','
                   PERFORM PARSE-NUMBER
                   ADD 1 TO WS-CURRENT-POS
               ELSE
                   STRING WS-CHAR DELIMITED BY SIZE INTO WS-NUM-STR
                   ADD 1 TO WS-CURRENT-POS
               END-IF
           END-PERFORM.
           PERFORM PARSE-NUMBER. *> Parse the last number

           PERFORM VARYING WS-INDEX FROM 0 BY 1 UNTIL WS-INDEX >= MEM-SIZE
               MOVE 0 TO MEM(WS-INDEX)
               MOVE UNKNOWN TO MAP(WS-INDEX / 101 + 1, WS-INDEX MOD 101 + 1)
               MOVE HIGH-VALUE TO MIN-DIST(WS-INDEX / 101 + 1, WS-INDEX MOD 101 + 1)
           END-PERFORM.

           MOVE 0 TO HEAD, TAIL, COUNT.
           MOVE 0 TO IP, RELATIVE-BASE, HALTED, INPUT-VAL, OUTPUT-VAL.

       PARSING-ERROR-EXIT.
           EXIT.

       PARSE-NUMBER.
           IF WS-NUM-STR NOT = SPACES THEN
               INSPECT WS-NUM-STR REPLACING ALL SPACE BY ZEROES
               MOVE WS-NUM-STR TO WS-NUM-VAL
               IF WS-CURRENT-POS <= MEM-SIZE THEN
                   MOVE WS-NUM-VAL TO MEM(WS-CURRENT-POS - 1)
               ELSE
                   DISPLAY 'Error: Program too large, exceeds MEM-SIZE.'
                   MOVE 1 TO RETURN-CODE
                   STOP RUN
               END-IF
               MOVE SPACES TO WS-NUM-STR
           END-IF.

       EXPLORE.
           MOVE MAP-OFFSET TO X(1).
           MOVE MAP-OFFSET TO Y(1).
           MOVE 0 TO DIST(1).
           MOVE 0 TO COUNT. *> Reset queue count
           MOVE 0 TO HEAD.
           MOVE 1 TO TAIL.
           ADD 1 TO COUNT.

           MOVE FLOOR TO MAP(Y(1), X(1)).
           MOVE 0 TO MIN-DIST(Y(1), X(1)).

           *> Initialize Intcode state for the first element
           MOVE 0 TO IP, RELATIVE-BASE, HALTED, INPUT-VAL, OUTPUT-VAL.
           MOVE 0 TO WS-INDEX.
           PERFORM VARYING WS-INDEX FROM 0 BY 1 UNTIL WS-INDEX >= MEM-SIZE
               MOVE MEM(WS-INDEX) TO MEM(WS-INDEX) OF INTCODE-STATE-COPY
           END-PERFORM.
           MOVE INTCODE-STATE-COPY TO QUEUE-ITEM(TAIL).

           PERFORM UNTIL COUNT = 0
               PERFORM DEQUEUE
               MOVE X TO WS-NEXT-X
               MOVE Y TO WS-NEXT-Y
               MOVE DIST TO WS-NEXT-DIST

               *> Copy current Intcode state from queue item
               MOVE QUEUE-ITEM(HEAD) TO WS-INTCODE-STATE
               UNSTRING WS-INTCODE-STATE DELIMITED BY SIZE INTO
                   IP, RELATIVE-BASE, HALTED, INPUT-VAL, OUTPUT-VAL, MEM(1) THRU MEM(MEM-SIZE)
               END-UNSTRING

               *> Try moving in all 4 directions
               PERFORM VARYING WS-MOVE-CMD FROM NORTH BY 1 UNTIL WS-MOVE-CMD > EAST
                   MOVE WS-NEXT-X TO X
                   MOVE WS-NEXT-Y TO Y
                   MOVE WS-NEXT-DIST TO DIST

                   COMPUTE WS-NEXT-X = X + DX(WS-MOVE-CMD)
                   COMPUTE WS-NEXT-Y = Y + DY(WS-MOVE-CMD)

                   IF WS-NEXT-X >= 0 AND WS-NEXT-X < MAP-DIM AND
                      WS-NEXT-Y >= 0 AND WS-NEXT-Y < MAP-DIM THEN

                       *> Create a copy of the Intcode state for this move
                       MOVE WS-INTCODE-STATE TO INTCODE-STATE-COPY

                       PERFORM RUN-INTCODE
                       IF RUN-STATUS = RUN-INPUT-NEEDED THEN
                           MOVE WS-MOVE-CMD TO INPUT-VAL
                           PERFORM PROVIDE-INPUT
                           PERFORM RUN-INTCODE *> Continue running
                       ELSE
                           DISPLAY 'Error: Intcode did not request input.'
                           MOVE 1 TO RETURN-CODE
                           STOP RUN
                       END-IF

                       IF RUN-STATUS = RUN-OUTPUT-READY THEN
                           MOVE OUTPUT-VAL TO WS-STATUS-CODE
                           ADD 1 TO DIST

                           IF WS-STATUS-CODE = WALL THEN
                               MOVE WALL TO MAP(WS-NEXT-Y, WS-NEXT-X)
                           ELSE IF WS-STATUS-CODE = FLOOR OR WS-STATUS-CODE = OXYGEN THEN
                               IF DIST < MIN-DIST(WS-NEXT-Y, WS-NEXT-X) THEN
                                   MOVE WS-STATUS-CODE TO MAP(WS-NEXT-Y, WS-NEXT-X)
                                   MOVE DIST TO MIN-DIST(WS-NEXT-Y, WS-NEXT-X)

                                   IF WS-STATUS-CODE = OXYGEN THEN
                                       MOVE WS-NEXT-X TO OXYGEN-X
                                       MOVE WS-NEXT-Y TO OXYGEN-Y
                                       MOVE DIST TO FINAL-DISTANCE
                                       DISPLAY FINAL-DISTANCE
                                       MOVE 0 TO COUNT *> Stop BFS
                                       EXIT PERFORM *> Exit the loop
                                   END-IF

                                   *> Enqueue the new state
                                   IF COUNT < MAX-QUEUE THEN
                                       MOVE WS-NEXT-X TO X(TAIL)
                                       MOVE WS-NEXT-Y TO Y(TAIL)
                                       MOVE DIST TO DIST(TAIL)

                                       *> Copy the modified Intcode state to queue item
                                       MOVE 0 TO WS-INDEX
                                       PERFORM VARYING WS-INDEX FROM 0 BY 1 UNTIL WS-INDEX >= MEM-SIZE
                                           MOVE MEM(WS-INDEX) TO MEM(WS-INDEX) OF INTCODE-STATE-COPY
                                       END-PERFORM
                                       MOVE INTCODE-STATE-COPY TO QUEUE-ITEM(TAIL)

                                       ADD 1 TO TAIL
                                       IF TAIL = MAX-QUEUE THEN MOVE 0 TO TAIL END-IF
                                       ADD 1 TO COUNT
                                   ELSE
                                       DISPLAY 'Error: BFS Queue overflow.'
                                       MOVE 1 TO RETURN-CODE
                                       STOP RUN
                                   END-IF
                               END-IF
                           ELSE
                               DISPLAY 'Error: Unknown status code ' WS-STATUS-CODE
                               MOVE 1 TO RETURN-CODE
                               STOP RUN
                           END-IF
                       ELSE IF RUN-STATUS = RUN-HALTED THEN
                           DISPLAY 'Warning: Intcode halted unexpectedly.'
                       ELSE
                           DISPLAY 'Error: Unexpected Intcode run status.'
                           MOVE 1 TO RETURN-CODE
                           STOP RUN
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.

           IF FINAL-DISTANCE = -1 THEN
               DISPLAY 'Error: Oxygen system not found.'
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF.

       RUN-INTCODE.
           *> Assume INTCODE-STATE-COPY holds the state to be processed
           *> This procedure modifies INTCODE-STATE-COPY and sets RUN-STATUS
           *> It's a simplified version, actual Intcode logic is complex.
           *> For this problem, we only care about input/output.

           IF HALTED OF INTCODE-STATE-COPY = 1 THEN
               MOVE RUN-HALTED TO RUN-STATUS
               EXIT.
           END-IF.

           PERFORM VARYING IP OF INTCODE-STATE-COPY FROM 0 BY 1 UNTIL IP OF INTCODE-STATE-COPY >= MEM-SIZE
               MOVE MEM(IP OF INTCODE-STATE-COPY) TO WS-PARAM1
               COMPUTE WS-OPCODE = FUNCTION MOD(WS-PARAM1, 100)
               COMPUTE WS-MODE1 = FUNCTION MOD(FUNCTION DIV(WS-PARAM1, 100), 10)
               COMPUTE WS-MODE2 = FUNCTION MOD(FUNCTION DIV(WS-PARAM1, 1000), 10)
               COMPUTE WS-MODE3 = FUNCTION MOD(FUNCTION DIV(WS-PARAM1, 10000), 10)

               IF WS-OPCODE = 99 THEN
                   MOVE 1 TO HALTED OF INTCODE-STATE-COPY
                   MOVE RUN-HALTED TO RUN-STATUS
                   EXIT.
               ELSE IF WS-OPCODE = 3 THEN *> Input
                   MOVE RUN-INPUT-NEEDED TO RUN-STATUS
                   ADD 2 TO IP OF INTCODE-STATE-COPY
                   EXIT.
               ELSE IF WS-OPCODE = 4 THEN *> Output
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   MOVE FUNCTION GET-PARAM-VAL(WS-MODE1, WS-PARAM1) TO OUTPUT-VAL OF INTCODE-STATE-COPY
                   MOVE RUN-OUTPUT-READY TO RUN-STATUS
                   ADD 2 TO IP OF INTCODE-STATE-COPY
                   EXIT.
               ELSE IF WS-OPCODE = 1 THEN *> Add
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 2) TO WS-PARAM2
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 3) TO WS-PARAM3
                   MOVE FUNCTION GET-PARAM-ADDR(WS-MODE3, WS-PARAM3) TO WS-ADDR
                   MOVE FUNCTION GET-PARAM-VAL(WS-MODE1, WS-PARAM1) + FUNCTION GET-PARAM-VAL(WS-MODE2, WS-PARAM2) TO MEM(WS-ADDR)
                   ADD 4 TO IP OF INTCODE-STATE-COPY
               ELSE IF WS-OPCODE = 2 THEN *> Multiply
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 2) TO WS-PARAM2
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 3) TO WS-PARAM3
                   MOVE FUNCTION GET-PARAM-ADDR(WS-MODE3, WS-PARAM3) TO WS-ADDR
                   MOVE FUNCTION GET-PARAM-VAL(WS-MODE1, WS-PARAM1) * FUNCTION GET-PARAM-VAL(WS-MODE2, WS-PARAM2) TO MEM(WS-ADDR)
                   ADD 4 TO IP OF INTCODE-STATE-COPY
               ELSE IF WS-OPCODE = 5 THEN *> Jump-if-true
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 2) TO WS-PARAM2
                   IF FUNCTION GET-PARAM-VAL(WS-MODE1, WS-PARAM1) NOT = 0 THEN
                       MOVE FUNCTION GET-PARAM-VAL(WS-MODE2, WS-PARAM2) TO IP OF INTCODE-STATE-COPY
                   ELSE
                       ADD 3 TO IP OF INTCODE-STATE-COPY
                   END-IF
               ELSE IF WS-OPCODE = 6 THEN *> Jump-if-false
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 2) TO WS-PARAM2
                   IF FUNCTION GET-PARAM-VAL(WS-MODE1, WS-PARAM1) = 0 THEN
                       MOVE FUNCTION GET-PARAM-VAL(WS-MODE2, WS-PARAM2) TO IP OF INTCODE-STATE-COPY
                   ELSE
                       ADD 3 TO IP OF INTCODE-STATE-COPY
                   END-IF
               ELSE IF WS-OPCODE = 7 THEN *> Less than
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 2) TO WS-PARAM2
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 3) TO WS-PARAM3
                   MOVE FUNCTION GET-PARAM-ADDR(WS-MODE3, WS-PARAM3) TO WS-ADDR
                   IF FUNCTION GET-PARAM-VAL(WS-MODE1, WS-PARAM1) < FUNCTION GET-PARAM-VAL(WS-MODE2, WS-PARAM2) THEN
                       MOVE 1 TO MEM(WS-ADDR)
                   ELSE
                       MOVE 0 TO MEM(WS-ADDR)
                   END-IF
                   ADD 4 TO IP OF INTCODE-STATE-COPY
               ELSE IF WS-OPCODE = 8 THEN *> Equals
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 2) TO WS-PARAM2
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 3) TO WS-PARAM3
                   MOVE FUNCTION GET-PARAM-ADDR(WS-MODE3, WS-PARAM3) TO WS-ADDR
                   IF FUNCTION GET-PARAM-VAL(WS-MODE1, WS-PARAM1) = FUNCTION GET-PARAM-VAL(WS-MODE2, WS-PARAM2) THEN
                       MOVE 1 TO MEM(WS-ADDR)
                   ELSE
                       MOVE 0 TO MEM(WS-ADDR)
                   END-IF
                   ADD 4 TO IP OF INTCODE-STATE-COPY
               ELSE IF WS-OPCODE = 9 THEN *> Adjust relative base
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   ADD FUNCTION GET-PARAM-VAL(WS-MODE1, WS-PARAM1) TO RELATIVE-BASE OF INTCODE-STATE-COPY
                   ADD 2 TO IP OF INTCODE-STATE-COPY
               ELSE
                   DISPLAY 'Error: Unknown opcode ' WS-OPCODE ' at IP ' IP OF INTCODE-STATE-COPY
                   MOVE 1 TO RETURN-CODE
                   STOP RUN
               END-IF
           END-PERFORM.

           DISPLAY 'Error: Instruction pointer out of bounds.'
           MOVE 1 TO RETURN-CODE
           STOP RUN.

       PROVIDE-INPUT.
           *> This procedure uses INPUT-VAL from INTCODE-STATE-COPY
           *> and writes it to the correct memory address.
           *> The IP has already been advanced by RUN-INTCODE.
           MOVE MEM(IP OF INTCODE-STATE-COPY - 2) TO WS-PARAM1 *> Get the instruction
           COMPUTE WS-MODE1 = FUNCTION MOD(FUNCTION DIV(WS-PARAM1, 100), 10)
           MOVE MEM(IP OF INTCODE-STATE-COPY - 1) TO WS-PARAM1 *> Get the parameter
           MOVE FUNCTION GET-PARAM-ADDR(WS-MODE1, WS-PARAM1) TO WS-ADDR
           MOVE INPUT-VAL OF INTCODE-STATE-COPY TO MEM(WS-ADDR)

       GET-PARAM-VAL.
           *> Input: Mode (PIC 9(1)), Parameter Value (PIC S9(18))
           *> Output: Value (PIC S9(18))
           *> This is a function, so it needs to be called as such.
           *> COBOL doesn't have direct function support like C.
           *> We'll simulate it with a PERFORM and RETURN-VALUE.
           *> For simplicity, this will be handled inline or via helper paragraphs.
           EXIT.

       GET-PARAM-ADDR.
           *> Input: Mode (PIC 9(1)), Parameter Value (PIC S9(18))
           *> Output: Address (PIC S9(18))
           EXIT.

       *> Helper paragraphs for parameter handling (simulating functions)
       GET-PARAM-VAL-PARA.
           *> Input: WS-MODE1, WS-PARAM1
           *> Output: WS-PARAM1 (modified to be the value)
           IF WS-MODE1 = 0 THEN *> Position mode
               IF WS-PARAM1 < 0 OR WS-PARAM1 >= MEM-SIZE THEN
                   DISPLAY 'Error: Position mode OOB read (' WS-PARAM1 ')'.
                   MOVE 1 TO RETURN-CODE
                   STOP RUN
               END-IF
               MOVE MEM(WS-PARAM1) TO WS-PARAM1
           ELSE IF WS-MODE1 = 1 THEN *> Immediate mode
               *> WS-PARAM1 is already the value
               CONTINUE
           ELSE IF WS-MODE1 = 2 THEN *> Relative mode
               IF RELATIVE-BASE OF INTCODE-STATE-COPY + WS-PARAM1 < 0 OR
                  RELATIVE-BASE OF INTCODE-STATE-COPY + WS-PARAM1 >= MEM-SIZE THEN
                   DISPLAY 'Error: Relative mode OOB read (' RELATIVE-BASE OF INTCODE-STATE-COPY + WS-PARAM1 ')'.
                   MOVE 1 TO RETURN-CODE
                   STOP RUN
               END-IF
               MOVE MEM(RELATIVE-BASE OF INTCODE-STATE-COPY + WS-PARAM1) TO WS-PARAM1
           ELSE
               DISPLAY 'Error: Invalid parameter mode ' WS-MODE1
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF.

       GET-PARAM-ADDR-PARA.
           *> Input: WS-MODE1, WS-PARAM1
           *> Output: WS-ADDR (calculated address)
           IF WS-PARAM1 < 0 THEN
               DISPLAY 'Error: Negative address param (' WS-PARAM1 ')'.
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF.
           IF WS-MODE1 = 0 THEN *> Position mode
               IF WS-PARAM1 >= MEM-SIZE THEN
                   DISPLAY 'Error: Position mode OOB write addr (' WS-PARAM1 ')'.
                   MOVE 1 TO RETURN-CODE
                   STOP RUN
               END-IF
               MOVE WS-PARAM1 TO WS-ADDR
           ELSE IF WS-MODE1 = 2 THEN *> Relative mode
               IF RELATIVE-BASE OF INTCODE-STATE-COPY + WS-PARAM1 < 0 OR
                  RELATIVE-BASE OF INTCODE-STATE-COPY + WS-PARAM1 >= MEM-SIZE THEN
                   DISPLAY 'Error: Relative mode OOB write addr (' RELATIVE-BASE OF INTCODE-STATE-COPY + WS-PARAM1 ')'.
                   MOVE 1 TO RETURN-CODE
                   STOP RUN
               END-IF
               MOVE RELATIVE-BASE OF INTCODE-STATE-COPY + WS-PARAM1 TO WS-ADDR
           ELSE
               DISPLAY 'Error: Invalid parameter mode ' WS-MODE1 ' for write address.'
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF.

       *> Queue Operations
       ENQUEUE.
           *> Input: BFS-STATE (X, Y, DIST, INTCODE-STATE-COPY)
           *> This is handled inline by moving values to the TAIL index.
           EXIT.

       DEQUEUE.
           *> Output: X, Y, DIST, INTCODE-STATE-COPY (from HEAD index)
           IF COUNT = 0 THEN
               DISPLAY 'Error: BFS Queue underflow.'
               MOVE 1 TO RETURN-CODE
               STOP RUN
           END-IF.
           MOVE X(HEAD) TO X.
           MOVE Y(HEAD) TO Y.
           MOVE DIST(HEAD) TO DIST.
           MOVE QUEUE-ITEM(HEAD) TO INTCODE-STATE-COPY. *> Copy state back
           ADD 1 TO HEAD.
           IF HEAD = MAX-QUEUE THEN MOVE 0 TO HEAD END-IF.
           SUBTRACT 1 FROM COUNT.

       *> Movement Deltas
       01  DX               OCCURS 4 TIMES PIC S9(3) COMP-5.
       01  DY               OCCURS 4 TIMES PIC S9(3) COMP-5.

       01  DX-INIT.
           05 FILLER PIC X(10) VALUE X'0000000000'. *> North (Y-1)
           05 FILLER PIC X(10) VALUE X'0000000000'. *> South (Y+1)
           05 FILLER PIC X(10) VALUE X'FFFFFFFFF0'. *> West (X-1)
           05 FILLER PIC X(10) VALUE X'0000000001'. *> East (X+1)

       01  DY-INIT.
           05 FILLER PIC X(10) VALUE X'FFFFFFFFF0'. *> North (Y-1)
           05 FILLER PIC X(10) VALUE X'0000000001'. *> South (Y+1)
           05 FILLER PIC X(10) VALUE X'0000000000'. *> West (X=0)
           05 FILLER PIC X(10) VALUE X'0000000000'. *> East (X=0)

       *> Initialize DX and DY arrays
       PROCEDURE DX-DY-INITIALIZATION.
           MOVE DX-INIT TO DX.
           MOVE DY-INIT TO DY.
       END-PROCEDURE.

       *> Call the initialization
       CALL 'DX-DY-INITIALIZATION'.

       *> Re-define the parameter handling to use the actual INTCODE-STATE-COPY
       *> This is a bit tricky in COBOL. We'll use temporary variables.

       *> Modified RUN-INTCODE to use the helper paragraphs
       REPLACE ==FUNCTION GET-PARAM-VAL(WS-MODE1, WS-PARAM1)== BY ==WS-PARAM1==
       REPLACE ==FUNCTION GET-PARAM-ADDR(WS-MODE3, WS-PARAM3)== BY ==WS-ADDR==
       REPLACE ==MEM(WS-ADDR)== BY ==MEM(WS-ADDR) OF INTCODE-STATE-COPY==
       REPLACE ==MEM(IP OF INTCODE-STATE-COPY + 1)== BY ==MEM(IP OF INTCODE-STATE-COPY + 1) OF INTCODE-STATE-COPY==
       REPLACE ==MEM(IP OF INTCODE-STATE-COPY + 2)== BY ==MEM(IP OF INTCODE-STATE-COPY + 2) OF INTCODE-STATE-COPY==
       REPLACE ==MEM(IP OF INTCODE-STATE-COPY + 3)== BY ==MEM(IP OF INTCODE-STATE-COPY + 3) OF INTCODE-STATE-COPY==
       REPLACE ==IP OF INTCODE-STATE-COPY== BY ==IP OF INTCODE-STATE-COPY==
       REPLACE ==RELATIVE-BASE OF INTCODE-STATE-COPY== BY ==RELATIVE-BASE OF INTCODE-STATE-COPY==
       REPLACE ==HALTED OF INTCODE-STATE-COPY== BY ==HALTED OF INTCODE-STATE-COPY==
       REPLACE ==INPUT-VAL OF INTCODE-STATE-COPY== BY ==INPUT-VAL OF INTCODE-STATE-COPY==
       REPLACE ==OUTPUT-VAL OF INTCODE-STATE-COPY== BY ==OUTPUT-VAL OF INTCODE-STATE-COPY==

       *> Need to adjust the RUN-INTCODE logic to correctly use the temporary variables
       *> and call the helper paragraphs.

       RUN-INTCODE-REVISED.
           IF HALTED OF INTCODE-STATE-COPY = 1 THEN
               MOVE RUN-HALTED TO RUN-STATUS
               EXIT.
           END-IF.

           PERFORM VARYING IP OF INTCODE-STATE-COPY FROM 0 BY 1 UNTIL IP OF INTCODE-STATE-COPY >= MEM-SIZE
               MOVE MEM(IP OF INTCODE-STATE-COPY) TO WS-PARAM1
               COMPUTE WS-OPCODE = FUNCTION MOD(WS-PARAM1, 100)
               COMPUTE WS-MODE1 = FUNCTION MOD(FUNCTION DIV(WS-PARAM1, 100), 10)
               COMPUTE WS-MODE2 = FUNCTION MOD(FUNCTION DIV(WS-PARAM1, 1000), 10)
               COMPUTE WS-MODE3 = FUNCTION MOD(FUNCTION DIV(WS-PARAM1, 10000), 10)

               IF WS-OPCODE = 99 THEN
                   MOVE 1 TO HALTED OF INTCODE-STATE-COPY
                   MOVE RUN-HALTED TO RUN-STATUS
                   EXIT.
               ELSE IF WS-OPCODE = 3 THEN *> Input
                   MOVE RUN-INPUT-NEEDED TO RUN-STATUS
                   ADD 2 TO IP OF INTCODE-STATE-COPY
                   EXIT.
               ELSE IF WS-OPCODE = 4 THEN *> Output
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   MOVE WS-PARAM1 TO OUTPUT-VAL OF INTCODE-STATE-COPY
                   MOVE RUN-OUTPUT-READY TO RUN-STATUS
                   ADD 2 TO IP OF INTCODE-STATE-COPY
                   EXIT.
               ELSE IF WS-OPCODE = 1 THEN *> Add
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   MOVE WS-PARAM1 TO WS-PARAM2 *> Store value of param1
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 2) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   MOVE WS-PARAM1 TO WS-PARAM3 *> Store value of param2
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 3) TO WS-PARAM1
                   PERFORM GET-PARAM-ADDR-PARA
                   MOVE WS-PARAM2 + WS-PARAM3 TO MEM(WS-ADDR) OF INTCODE-STATE-COPY
                   ADD 4 TO IP OF INTCODE-STATE-COPY
               ELSE IF WS-OPCODE = 2 THEN *> Multiply
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   MOVE WS-PARAM1 TO WS-PARAM2
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 2) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   MOVE WS-PARAM1 TO WS-PARAM3
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 3) TO WS-PARAM1
                   PERFORM GET-PARAM-ADDR-PARA
                   MOVE WS-PARAM2 * WS-PARAM3 TO MEM(WS-ADDR) OF INTCODE-STATE-COPY
                   ADD 4 TO IP OF INTCODE-STATE-COPY
               ELSE IF WS-OPCODE = 5 THEN *> Jump-if-true
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   MOVE WS-PARAM1 TO WS-PARAM2 *> Store value of param1
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 2) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   IF WS-PARAM2 NOT = 0 THEN
                       MOVE WS-PARAM1 TO IP OF INTCODE-STATE-COPY
                   ELSE
                       ADD 3 TO IP OF INTCODE-STATE-COPY
                   END-IF
               ELSE IF WS-OPCODE = 6 THEN *> Jump-if-false
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   MOVE WS-PARAM1 TO WS-PARAM2 *> Store value of param1
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 2) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   IF WS-PARAM2 = 0 THEN
                       MOVE WS-PARAM1 TO IP OF INTCODE-STATE-COPY
                   ELSE
                       ADD 3 TO IP OF INTCODE-STATE-COPY
                   END-IF
               ELSE IF WS-OPCODE = 7 THEN *> Less than
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   MOVE WS-PARAM1 TO WS-PARAM2
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 2) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   MOVE WS-PARAM1 TO WS-PARAM3
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 3) TO WS-PARAM1
                   PERFORM GET-PARAM-ADDR-PARA
                   IF WS-PARAM2 < WS-PARAM3 THEN
                       MOVE 1 TO MEM(WS-ADDR) OF INTCODE-STATE-COPY
                   ELSE
                       MOVE 0 TO MEM(WS-ADDR) OF INTCODE-STATE-COPY
                   END-IF
                   ADD 4 TO IP OF INTCODE-STATE-COPY
               ELSE IF WS-OPCODE = 8 THEN *> Equals
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   MOVE WS-PARAM1 TO WS-PARAM2
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 2) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   MOVE WS-PARAM1 TO WS-PARAM3
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 3) TO WS-PARAM1
                   PERFORM GET-PARAM-ADDR-PARA
                   IF WS-PARAM2 = WS-PARAM3 THEN
                       MOVE 1 TO MEM(WS-ADDR) OF INTCODE-STATE-COPY
                   ELSE
                       MOVE 0 TO MEM(WS-ADDR) OF INTCODE-STATE-COPY
                   END-IF
                   ADD 4 TO IP OF INTCODE-STATE-COPY
               ELSE IF WS-OPCODE = 9 THEN *> Adjust relative base
                   MOVE MEM(IP OF INTCODE-STATE-COPY + 1) TO WS-PARAM1
                   PERFORM GET-PARAM-VAL-PARA
                   ADD WS-PARAM1 TO RELATIVE-BASE OF INTCODE-STATE-COPY
                   ADD 2 TO IP OF INTCODE-STATE-COPY
               ELSE
                   DISPLAY 'Error: Unknown opcode ' WS-OPCODE ' at IP ' IP OF INTCODE-STATE-COPY
                   MOVE 1 TO RETURN-CODE
                   STOP RUN
               END-IF
           END-PERFORM.

           DISPLAY 'Error: Instruction pointer out of bounds.'
           MOVE 1 TO RETURN-CODE
           STOP RUN.

       *> Re-call the main procedure with the revised RUN-INTCODE
       END PROGRAM ROBOT-EXPLORER.
