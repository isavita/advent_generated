
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTCODE-VM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  INPUT-FILENAME      PIC X(12) VALUE "input.txt".
       01  CODE-MEMORY.
           05  CODE-CELL OCCURS 10000 TIMES PIC S9(9) COMP-5.

       01  VM-STATE.
           05  VM-IP           PIC S9(9) COMP-5.
           05  VM-RELATIVE-BASE PIC S9(9) COMP-5.
           05  VM-INPUT OCCURS 2 TIMES PIC S9(9) COMP-5.
           05  VM-OUTPUT       PIC S9(9) COMP-5.

       01  FILE-STATUS         PIC X(2).
       01  READ-BUFFER         PIC X(1000).
       01  TOKEN               PIC X(10).
       01  TOKEN-PTR           PIC S9(9) COMP-5.
       01  TOKEN-LEN           PIC S9(9) COMP-5.
       01  CURRENT-CODE-INDEX  PIC S9(9) COMP-5.

       01  CMD                 PIC S9(9) COMP-5.
       01  OP-CODE             PIC S9(9) COMP-5.
       01  MODES OCCURS 3 TIMES PIC S9(9) COMP-5.
       01  PARAMS OCCURS 3 TIMES PIC S9(9) COMP-5.
       01  ARITY               PIC S9(9) COMP-5.

       01  X-COORD             PIC S9(4) COMP-5.
       01  Y-COORD             PIC S9(4) COMP-5.
       01  SUM                 PIC S9(9) COMP-5 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM LOAD-CODE.
           PERFORM PROCESS-GRID.
           DISPLAY SUM.
           STOP RUN.

       LOAD-CODE.
           OPEN INPUT INPUT-FILENAME.
           IF FILE-STATUS NOT = '00'
               DISPLAY "Failed to open file: " INPUT-FILENAME
               STOP RUN
           END-IF.

           READ INPUT-FILENAME INTO READ-BUFFER
               AT END
                   DISPLAY "Empty input file."
                   CLOSE INPUT-FILENAME
                   STOP RUN
               NOT AT END
                   MOVE 0 TO CURRENT-CODE-INDEX
                   MOVE READ-BUFFER TO TOKEN
                   CALL 'STRTOK' USING TOKEN BY REFERENCE
                                       TOKEN-PTR BY VALUE
                                       TOKEN-LEN BY VALUE
                                       TOKEN BY REFERENCE
                                       TOKEN-LEN BY VALUE
                   PERFORM UNTIL TOKEN-LEN = 0
                       CALL 'ATOI' USING TOKEN BY REFERENCE
                                           PARAMS(1) BY VALUE
                       MOVE PARAMS(1) TO CODE-CELL(CURRENT-CODE-INDEX)
                       ADD 1 TO CURRENT-CODE-INDEX
                       CALL 'STRTOK' USING TOKEN BY REFERENCE
                                           TOKEN-PTR BY VALUE
                                           TOKEN-LEN BY VALUE
                                           TOKEN BY REFERENCE
                                           TOKEN-LEN BY VALUE
                   END-PERFORM
                   CLOSE INPUT-FILENAME
           END-READ.

       PROCESS-GRID.
           MOVE 0 TO Y-COORD.
           PERFORM UNTIL Y-COORD = 50
               MOVE 0 TO X-COORD
               PERFORM UNTIL X-COORD = 50
                   MOVE X-COORD TO VM-INPUT(1)
                   MOVE Y-COORD TO VM-INPUT(2)
                   PERFORM RUN-VM
                   IF VM-OUTPUT = 1
                       ADD 1 TO SUM
                   END-IF
                   ADD 1 TO X-COORD
               END-PERFORM
               ADD 1 TO Y-COORD
           END-PERFORM.

       RUN-VM.
           MOVE 0 TO VM-IP.
           MOVE 0 TO VM-RELATIVE-BASE.
           PERFORM UNTIL OP-CODE = 99
               MOVE CODE-CELL(VM-IP) TO CMD
               COMPUTE OP-CODE = FUNCTION MOD(CMD, 100)
               COMPUTE MODES(1) = FUNCTION MOD(FUNCTION DIV(CMD, 100), 10)
               COMPUTE MODES(2) = FUNCTION MOD(FUNCTION DIV(CMD, 1000), 10)
               COMPUTE MODES(3) = FUNCTION MOD(FUNCTION DIV(CMD, 10000), 10)

               EVALUATE OP-CODE
                   WHEN 1
                       MOVE 3 TO ARITY
                       PERFORM GET-PARAM-ADDRESS
                       PERFORM GET-PARAM-ADDRESS
                       PERFORM GET-PARAM-ADDRESS
                       MOVE FUNCTION ADD(CODE-CELL(PARAMS(1)), CODE-CELL(PARAMS(2)))
                           TO CODE-CELL(PARAMS(3))
                   WHEN 2
                       MOVE 3 TO ARITY
                       PERFORM GET-PARAM-ADDRESS
                       PERFORM GET-PARAM-ADDRESS
                       PERFORM GET-PARAM-ADDRESS
                       MOVE FUNCTION MULT(CODE-CELL(PARAMS(1)), CODE-CELL(PARAMS(2)))
                           TO CODE-CELL(PARAMS(3))
                   WHEN 3
                       MOVE 1 TO ARITY
                       PERFORM GET-PARAM-ADDRESS
                       MOVE VM-INPUT(1) TO CODE-CELL(PARAMS(1))
                       MOVE VM-INPUT(2) TO VM-INPUT(1)
                   WHEN 4
                       MOVE 1 TO ARITY
                       PERFORM GET-PARAM-ADDRESS
                       MOVE CODE-CELL(PARAMS(1)) TO VM-OUTPUT
                       GO TO END-VM-LOOP
                   WHEN 5
                       MOVE 2 TO ARITY
                       PERFORM GET-PARAM-ADDRESS
                       PERFORM GET-PARAM-ADDRESS
                       IF CODE-CELL(PARAMS(1)) NOT = 0
                           MOVE CODE-CELL(PARAMS(2)) TO VM-IP
                           GO TO NEXT-INSTRUCTION
                       END-IF
                   WHEN 6
                       MOVE 2 TO ARITY
                       PERFORM GET-PARAM-ADDRESS
                       PERFORM GET-PARAM-ADDRESS
                       IF CODE-CELL(PARAMS(1)) = 0
                           MOVE CODE-CELL(PARAMS(2)) TO VM-IP
                           GO TO NEXT-INSTRUCTION
                       END-IF
                   WHEN 7
                       MOVE 3 TO ARITY
                       PERFORM GET-PARAM-ADDRESS
                       PERFORM GET-PARAM-ADDRESS
                       PERFORM GET-PARAM-ADDRESS
                       IF CODE-CELL(PARAMS(1)) < CODE-CELL(PARAMS(2))
                           MOVE 1 TO CODE-CELL(PARAMS(3))
                       ELSE
                           MOVE 0 TO CODE-CELL(PARAMS(3))
                       END-IF
                   WHEN 8
                       MOVE 3 TO ARITY
                       PERFORM GET-PARAM-ADDRESS
                       PERFORM GET-PARAM-ADDRESS
                       PERFORM GET-PARAM-ADDRESS
                       IF CODE-CELL(PARAMS(1)) = CODE-CELL(PARAMS(2))
                           MOVE 1 TO CODE-CELL(PARAMS(3))
                       ELSE
                           MOVE 0 TO CODE-CELL(PARAMS(3))
                       END-IF
                   WHEN 9
                       MOVE 1 TO ARITY
                       PERFORM GET-PARAM-ADDRESS
                       ADD CODE-CELL(PARAMS(1)) TO VM-RELATIVE-BASE
                   WHEN 99
                       GO TO END-VM-LOOP
                   WHEN OTHER
                       DISPLAY "Unknown opcode: " OP-CODE
                       STOP RUN
               END-EVALUATE.

               ADD ARITY TO VM-IP
               ADD 1 TO VM-IP.
           END-PERFORM.
       END-VM-LOOP.
           EXIT.
       NEXT-INSTRUCTION.
           EXIT.

       GET-PARAM-ADDRESS.
           ADD 1 TO VM-IP.
           COMPUTE PARAMS(FUNCTION ORD(VM-IP)) =
               FUNCTION IF(MODES(FUNCTION ORD(VM-IP)) = 0,
                           CODE-CELL(VM-IP),
                           FUNCTION IF(MODES(FUNCTION ORD(VM-IP)) = 1,
                                       VM-IP,
                                       FUNCTION ADD(VM-RELATIVE-BASE, CODE-CELL(VM-IP))))
               BY VALUE.

       END PROGRAM INTCODE-VM.

      ******************************************************************
      * Subroutine to emulate C's strtok
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRTOK.
       DATA DIVISION.
       LINKAGE SECTION.
       01  ARG-STRING          PIC X(1000).
       01  ARG-PTR             PIC S9(9) COMP-5.
       01  ARG-LEN             PIC S9(9) COMP-5.
       01  ARG-DELIMITER       PIC X(10).
       01  ARG-DELIM-LEN       PIC S9(9) COMP-5.
       01  WS-STRING           PIC X(1000).
       01  WS-DELIMITER-POS    PIC S9(9) COMP-5.
       01  WS-CURRENT-POS      PIC S9(9) COMP-5.
       01  WS-END-POS          PIC S9(9) COMP-5.
       01  WS-DELIMITER-CHAR   PIC X.

       PROCEDURE DIVISION USING ARG-STRING, ARG-PTR, ARG-LEN,
                                ARG-DELIMITER, ARG-DELIM-LEN.
       STRTOK-ENTRY.
           MOVE ARG-STRING TO WS-STRING.
           MOVE 1 TO WS-CURRENT-POS.
           MOVE FUNCTION LENGTH(WS-STRING) TO WS-END-POS.

           IF ARG-PTR = 0
               MOVE 1 TO WS-CURRENT-POS
           ELSE
               MOVE ARG-PTR TO WS-CURRENT-POS
           END-IF.

           PERFORM VARYING WS-CURRENT-POS FROM WS-CURRENT-POS BY 1
               UNTIL WS-CURRENT-POS > WS-END-POS OR
                     WS-STRING(WS-CURRENT-POS:1) NOT = ','
           END-PERFORM.

           IF WS-CURRENT-POS > WS-END-POS
               MOVE 0 TO ARG-LEN
               MOVE 0 TO ARG-PTR
               EXIT PROGRAM
           END-IF.

           MOVE WS-CURRENT-POS TO ARG-PTR.
           MOVE WS-CURRENT-POS TO WS-DELIMITER-POS.

           PERFORM VARYING WS-CURRENT-POS FROM WS-CURRENT-POS BY 1
               UNTIL WS-CURRENT-POS > WS-END-POS OR
                     WS-STRING(WS-CURRENT-POS:1) = ','
           END-PERFORM.

           IF WS-CURRENT-POS > WS-END-POS
               MOVE WS-STRING(ARG-PTR:) TO ARG-STRING
               MOVE FUNCTION LENGTH(ARG-STRING) TO ARG-LEN
               MOVE 0 TO ARG-PTR
           ELSE
               MOVE WS-STRING(ARG-PTR:WS-CURRENT-POS - ARG-PTR) TO ARG-STRING
               MOVE FUNCTION LENGTH(ARG-STRING) TO ARG-LEN
               MOVE WS-CURRENT-POS TO ARG-PTR
           END-IF.
           EXIT PROGRAM.

      ******************************************************************
      * Subroutine to emulate C's atoi
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATOI.
       DATA DIVISION.
       LINKAGE SECTION.
       01  ARG-STRING          PIC X(10).
       01  ARG-INT             PIC S9(9) COMP-5.
       01  WS-STRING           PIC X(10).
       01  WS-INT              PIC S9(9) COMP-5.
       01  WS-CHAR             PIC X.
       01  WS-SIGN             PIC S9(9) COMP-5 VALUE 1.
       01  WS-INDEX            PIC S9(9) COMP-5.

       PROCEDURE DIVISION USING ARG-STRING BY VALUE, ARG-INT BY REFERENCE.
       ATOI-ENTRY.
           MOVE ARG-STRING TO WS-STRING.
           MOVE 0 TO WS-INT.
           MOVE 1 TO WS-INDEX.

           IF WS-STRING(1:1) = '-'
               MOVE -1 TO WS-SIGN
               MOVE 2 TO WS-INDEX
           ELSE IF WS-STRING(1:1) = '+'
               MOVE 2 TO WS-INDEX
           END-IF.

           PERFORM VARYING WS-INDEX FROM WS-INDEX BY 1
               UNTIL WS-INDEX > FUNCTION LENGTH(WS-STRING)
               MOVE WS-STRING(WS-INDEX:1) TO WS-CHAR
               IF WS-CHAR IS NUMERIC
                   MULTIPLY WS-INT BY 10 GIVING WS-INT
                   ADD FUNCTION NUMVAL(WS-CHAR) TO WS-INT
               ELSE
                   GO TO END-ATOI
               END-IF
           END-PERFORM.

       END-ATOI.
           MULTIPLY WS-INT BY WS-SIGN GIVING ARG-INT.
           EXIT PROGRAM.
