
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TURING-MACHINE.
       AUTHOR. Programmer.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD            PIC X(20).

       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC A(1) VALUE 'N'.
           88 EOF                           VALUE 'Y'.

       01  WS-REGISTERS.
           05 WS-REG-A              PIC S9(18) COMP VALUE 0.
           05 WS-REG-B              PIC S9(18) COMP VALUE 0.

       01  WS-PROGRAM-CONTROL.
           05 WS-PC                 PIC 9(4) COMP VALUE 1.
           05 WS-INSTRUCTION-COUNT  PIC 9(4) COMP VALUE 0.

       01  WS-INSTRUCTION-TABLE.
           05 WS-INSTRUCTION OCCURS 100 TIMES
                                INDEXED BY INSTR-IDX.
               10 WS-OPCODE         PIC X(3).
               10 WS-REG            PIC X(1).
               10 WS-OFFSET         PIC S9(4) COMP.

       01  WS-TEMP-VARS.
           05 WS-TEMP-OFFSET-STR    PIC X(10).
           05 WS-TEMP-OFFSET-NUM    PIC S9(4).
           05 WS-TEMP-REG-VAL       PIC S9(18) COMP.
           05 WS-REMAINDER          PIC 9(1) COMP.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM LOAD-INSTRUCTIONS.
           PERFORM EXECUTE-PROGRAM
               UNTIL WS-PC < 1 OR WS-PC > WS-INSTRUCTION-COUNT.
           DISPLAY WS-REG-B.
           STOP RUN.

       LOAD-INSTRUCTIONS.
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL EOF
               READ INPUT-FILE
                   AT END SET EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-INSTRUCTION-COUNT
                       SET INSTR-IDX TO WS-INSTRUCTION-COUNT
                       PERFORM PARSE-INSTRUCTION
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.

       PARSE-INSTRUCTION.
           MOVE INPUT-RECORD(1:3) TO WS-OPCODE(INSTR-IDX).
           EVALUATE WS-OPCODE(INSTR-IDX)
               WHEN "hlf"
               WHEN "tpl"
               WHEN "inc"
                   MOVE INPUT-RECORD(5:1) TO WS-REG(INSTR-IDX)
                   MOVE 0 TO WS-OFFSET(INSTR-IDX)
               WHEN "jmp"
                   MOVE SPACES TO WS-REG(INSTR-IDX)
                   MOVE FUNCTION TRIM(INPUT-RECORD(5:))
                     TO WS-TEMP-OFFSET-STR
                   COMPUTE WS-TEMP-OFFSET-NUM =
                           FUNCTION NUMVAL(WS-TEMP-OFFSET-STR)
                   MOVE WS-TEMP-OFFSET-NUM TO WS-OFFSET(INSTR-IDX)
               WHEN "jie"
               WHEN "jio"
                   MOVE INPUT-RECORD(5:1) TO WS-REG(INSTR-IDX)
                   MOVE FUNCTION TRIM(INPUT-RECORD(8:))
                     TO WS-TEMP-OFFSET-STR
                   COMPUTE WS-TEMP-OFFSET-NUM =
                           FUNCTION NUMVAL(WS-TEMP-OFFSET-STR)
                   MOVE WS-TEMP-OFFSET-NUM TO WS-OFFSET(INSTR-IDX)
           END-EVALUATE.

       EXECUTE-PROGRAM.
           SET INSTR-IDX TO WS-PC.
           EVALUATE WS-OPCODE(INSTR-IDX)
               WHEN "hlf"
                   IF WS-REG(INSTR-IDX) = "a"
                       COMPUTE WS-REG-A = WS-REG-A / 2
                   ELSE
                       COMPUTE WS-REG-B = WS-REG-B / 2
                   END-IF
                   ADD 1 TO WS-PC
               WHEN "tpl"
                   IF WS-REG(INSTR-IDX) = "a"
                       COMPUTE WS-REG-A = WS-REG-A * 3
                   ELSE
                       COMPUTE WS-REG-B = WS-REG-B * 3
                   END-IF
                   ADD 1 TO WS-PC
               WHEN "inc"
                   IF WS-REG(INSTR-IDX) = "a"
                       ADD 1 TO WS-REG-A
                   ELSE
                       ADD 1 TO WS-REG-B
                   END-IF
                   ADD 1 TO WS-PC
               WHEN "jmp"
                   ADD WS-OFFSET(INSTR-IDX) TO WS-PC
               WHEN "jie"
                   IF WS-REG(INSTR-IDX) = "a"
                       MOVE WS-REG-A TO WS-TEMP-REG-VAL
                   ELSE
                       MOVE WS-REG-B TO WS-TEMP-REG-VAL
                   END-IF
                   DIVIDE WS-TEMP-REG-VAL BY 2 GIVING WS-TEMP-REG-VAL
                                            REMAINDER WS-REMAINDER
                   IF WS-REMAINDER = 0
                       ADD WS-OFFSET(INSTR-IDX) TO WS-PC
                   ELSE
                       ADD 1 TO WS-PC
                   END-IF
               WHEN "jio"
                   IF WS-REG(INSTR-IDX) = "a"
                       MOVE WS-REG-A TO WS-TEMP-REG-VAL
                   ELSE
                       MOVE WS-REG-B TO WS-TEMP-REG-VAL
                   END-IF
                   IF WS-TEMP-REG-VAL = 1
                       ADD WS-OFFSET(INSTR-IDX) TO WS-PC
                   ELSE
                       ADD 1 TO WS-PC
                   END-IF
           END-EVALUATE.
