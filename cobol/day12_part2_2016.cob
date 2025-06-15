
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Assembunny.
       AUTHOR. Programmer.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD          PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-EOF                PIC X VALUE 'N'.
       01  WS-LINE               PIC X(100).

       01  WS-COUNTERS.
           05 WS-NUM-INSTRUCTIONS PIC 9(4) VALUE 0.
           05 WS-PC               PIC S9(9) COMP.
           05 WS-PART             PIC 9.
           05 WS-REG-IDX          PIC 9 COMP.

       01  WS-VALUES.
           05 WS-VAL1             PIC S9(9) COMP.
           05 WS-VAL2             PIC S9(9) COMP.

       01  WS-HELPER-VARS.
           05 WS-ARG-IN           PIC X(10).
           05 WS-ARG-OUT          PIC S9(9) COMP.
           05 WS-DISPLAY-A        PIC -Z(8)9.

       01  INSTRUCTION-PARTS.
           05 WS-OP               PIC X(4).
           05 WS-ARG1             PIC X(10).
           05 WS-ARG2             PIC X(10).

       01  INSTRUCTION-TABLE.
           05 INSTRUCTIONS OCCURS 1000 TIMES
              INDEXED BY INSTR-IDX.
              10 INSTR-OP         PIC X(4).
              10 INSTR-ARG1       PIC X(10).
              10 INSTR-ARG2       PIC X(10).

       01  REGISTERS-TABLE.
           05 REGISTERS OCCURS 4 TIMES PIC S9(9) COMP.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-READ-INSTRUCTIONS.
           PERFORM VARYING WS-PART FROM 1 BY 1 UNTIL WS-PART > 2
               PERFORM 200-RUN-SIMULATION
               MOVE REGISTERS(1) TO WS-DISPLAY-A
               DISPLAY "Part " WS-PART ": Register a = "
                   FUNCTION TRIM(WS-DISPLAY-A)
           END-PERFORM.
           STOP RUN.

       100-READ-INSTRUCTIONS.
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL WS-EOF = 'Y'
               READ INPUT-FILE INTO WS-LINE
                   AT END
                       SET WS-EOF TO 'Y'
                   NOT AT END
                       ADD 1 TO WS-NUM-INSTRUCTIONS
                       INITIALIZE INSTRUCTION-PARTS
                       UNSTRING WS-LINE DELIMITED BY SPACE
                           INTO WS-OP, WS-ARG1, WS-ARG2
                       END-UNSTRING
                       MOVE INSTRUCTION-PARTS TO
                           INSTRUCTIONS(WS-NUM-INSTRUCTIONS)
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.

       200-RUN-SIMULATION.
           INITIALIZE REGISTERS-TABLE.
           IF WS-PART = 2
               MOVE 1 TO REGISTERS(3)
           END-IF.
           MOVE 1 TO WS-PC.
           PERFORM UNTIL WS-PC < 1 OR WS-PC > WS-NUM-INSTRUCTIONS
               SET INSTR-IDX TO WS-PC
               EVALUATE INSTR-OP(INSTR-IDX)
                   WHEN "cpy"
                       MOVE INSTR-ARG1(INSTR-IDX) TO WS-ARG-IN
                       PERFORM 300-GET-VALUE
                       MOVE WS-ARG-OUT TO WS-VAL1
                       MOVE INSTR-ARG2(INSTR-IDX) TO WS-ARG-IN
                       PERFORM 310-GET-REG-INDEX
                       IF WS-REG-IDX > 0
                           MOVE WS-VAL1 TO REGISTERS(WS-REG-IDX)
                       END-IF
                       ADD 1 TO WS-PC
                   WHEN "inc"
                       MOVE INSTR-ARG1(INSTR-IDX) TO WS-ARG-IN
                       PERFORM 310-GET-REG-INDEX
                       IF WS-REG-IDX > 0
                           ADD 1 TO REGISTERS(WS-REG-IDX)
                       END-IF
                       ADD 1 TO WS-PC
                   WHEN "dec"
                       MOVE INSTR-ARG1(INSTR-IDX) TO WS-ARG-IN
                       PERFORM 310-GET-REG-INDEX
                       IF WS-REG-IDX > 0
                           SUBTRACT 1 FROM REGISTERS(WS-REG-IDX)
                       END-IF
                       ADD 1 TO WS-PC
                   WHEN "jnz"
                       MOVE INSTR-ARG1(INSTR-IDX) TO WS-ARG-IN
                       PERFORM 300-GET-VALUE
                       MOVE WS-ARG-OUT TO WS-VAL1
                       IF WS-VAL1 NOT = 0
                           MOVE INSTR-ARG2(INSTR-IDX) TO WS-ARG-IN
                           PERFORM 300-GET-VALUE
                           MOVE WS-ARG-OUT TO WS-VAL2
                           ADD WS-VAL2 TO WS-PC
                       ELSE
                           ADD 1 TO WS-PC
                       END-IF
               END-EVALUATE
           END-PERFORM.

       300-GET-VALUE.
           EVALUATE WS-ARG-IN(1:1)
               WHEN 'a' MOVE REGISTERS(1) TO WS-ARG-OUT
               WHEN 'b' MOVE REGISTERS(2) TO WS-ARG-OUT
               WHEN 'c' MOVE REGISTERS(3) TO WS-ARG-OUT
               WHEN 'd' MOVE REGISTERS(4) TO WS-ARG-OUT
               WHEN OTHER
                   COMPUTE WS-ARG-OUT = FUNCTION NUMVAL(WS-ARG-IN)
           END-EVALUATE.

       310-GET-REG-INDEX.
           MOVE 0 TO WS-REG-IDX.
           EVALUATE WS-ARG-IN(1:1)
               WHEN 'a' MOVE 1 TO WS-REG-IDX
               WHEN 'b' MOVE 2 TO WS-REG-IDX
               WHEN 'c' MOVE 3 TO WS-REG-IDX
               WHEN 'd' MOVE 4 TO WS-REG-IDX
           END-EVALUATE.
