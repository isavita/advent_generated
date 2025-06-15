
       IDENTIFICATION DIVISION.
       PROGRAM-ID. solve.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD           PIC X(20).

       WORKING-STORAGE SECTION.
       01  WS-EOF                 PIC A(1) VALUE 'N'.
           88 FILE-AT-END                  VALUE 'Y'.

       01  REGISTERS.
           05 REG-A               PIC S9(10) VALUE 0.
           05 REG-B               PIC S9(10) VALUE 0.
           05 REG-C               PIC S9(10) VALUE 0.
           05 REG-D               PIC S9(10) VALUE 0.

       01  INSTRUCTIONS-TABLE.
           05 INSTR-LINE OCCURS 100 TIMES INDEXED BY INSTR-IDX.
              10 INSTR-TEXT       PIC X(20).

       01  PROGRAM-COUNTERS.
           05 PC                  PIC 9(4) COMP.
           05 INSTR-COUNT         PIC 9(4) COMP VALUE 0.

       01  PARSED-INSTRUCTION.
           05 OP-CODE             PIC X(3).
           05 ARG1                PIC X(10).
           05 ARG2                PIC X(10).

       01  TEMP-VALUES.
           05 VAL1                PIC S9(10).
           05 VAL2                PIC S9(10).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM READ-INSTRUCTIONS.
           PERFORM EXECUTE-PROGRAM.
           DISPLAY FUNCTION TRIM(REG-A).
           STOP RUN.

       READ-INSTRUCTIONS.
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL FILE-AT-END
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END SET FILE-AT-END TO TRUE
                   NOT AT END
                       ADD 1 TO INSTR-COUNT
                       MOVE INPUT-RECORD TO INSTR-TEXT(INSTR-COUNT)
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.

       EXECUTE-PROGRAM.
           MOVE 1 TO PC.
           PERFORM UNTIL PC > INSTR-COUNT
               MOVE SPACES TO PARSED-INSTRUCTION
               UNSTRING INSTR-TEXT(PC) DELIMITED BY SPACE
                   INTO OP-CODE, ARG1, ARG2
               END-UNSTRING
               EVALUATE OP-CODE
                   WHEN "cpy" PERFORM CPY-ROUTINE
                   WHEN "inc" PERFORM INC-ROUTINE
                   WHEN "dec" PERFORM DEC-ROUTINE
                   WHEN "jnz" PERFORM JNZ-ROUTINE
               END-EVALUATE
           END-PERFORM.

       GET-ARG-VALUE.
           EVALUATE ARG1
               WHEN "a" MOVE REG-A TO VAL1
               WHEN "b" MOVE REG-B TO VAL1
               WHEN "c" MOVE REG-C TO VAL1
               WHEN "d" MOVE REG-D TO VAL1
               WHEN OTHER COMPUTE VAL1 = FUNCTION NUMVAL(ARG1)
           END-EVALUATE.

       CPY-ROUTINE.
           PERFORM GET-ARG-VALUE.
           EVALUATE ARG2
               WHEN "a" MOVE VAL1 TO REG-A
               WHEN "b" MOVE VAL1 TO REG-B
               WHEN "c" MOVE VAL1 TO REG-C
               WHEN "d" MOVE VAL1 TO REG-D
           END-EVALUATE.
           ADD 1 TO PC.

       INC-ROUTINE.
           EVALUATE ARG1
               WHEN "a" ADD 1 TO REG-A
               WHEN "b" ADD 1 TO REG-B
               WHEN "c" ADD 1 TO REG-C
               WHEN "d" ADD 1 TO REG-D
           END-EVALUATE.
           ADD 1 TO PC.

       DEC-ROUTINE.
           EVALUATE ARG1
               WHEN "a" SUBTRACT 1 FROM REG-A
               WHEN "b" SUBTRACT 1 FROM REG-B
               WHEN "c" SUBTRACT 1 FROM REG-C
               WHEN "d" SUBTRACT 1 FROM REG-D
           END-EVALUATE.
           ADD 1 TO PC.

       JNZ-ROUTINE.
           PERFORM GET-ARG-VALUE.
           IF VAL1 IS NOT ZERO
               COMPUTE VAL2 = FUNCTION NUMVAL(ARG2)
               ADD VAL2 TO PC
           ELSE
               ADD 1 TO PC
           END-IF.
