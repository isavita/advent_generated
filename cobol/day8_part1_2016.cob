
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-LIGHTS.
       AUTHOR. EXPERT PROGRAMMER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'input.txt'
               ORGANIZATION IS SEQUENTIAL
               ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           05 FILLER PIC X(50).

       WORKING-STORAGE SECTION.
       01 SCREEN-WIDTH         PIC 9(2) VALUE 50.
       01 SCREEN-HEIGHT        PIC 9(1) VALUE 6.

       01 SCREEN-DATA.
           05 SCREEN-ROW OCCURS 6 TIMES.
              10 SCREEN-PIXEL OCCURS 50 TIMES PIC X.

       01 WS-INSTRUCTION       PIC X(50).
       01 WS-CMD               PIC X(10).
       01 WS-PARAM1            PIC 9(2).
       01 WS-PARAM2            PIC 9(2).
       01 WS-I                 PIC 9(2).
       01 WS-J                 PIC 9(2).
       01 WS-K                 PIC 9(2).
       01 WS-TEMP-ROW          PIC X(50).
       01 WS-TEMP-COL          OCCURS 6 TIMES PIC X.
       01 WS-LIT-COUNT         PIC 9(4) VALUE 0.
       01 WS-EOF               PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-SCREEN.
           PERFORM PROCESS-INPUT-FILE.
           PERFORM COUNT-LIT-PIXELS.
           DISPLAY WS-LIT-COUNT.
           STOP RUN.

       INITIALIZE-SCREEN.
           MOVE SPACES TO SCREEN-DATA.

       PROCESS-INPUT-FILE.
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL WS-EOF = 'Y'
               READ INPUT-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE INPUT-RECORD TO WS-INSTRUCTION
                       PERFORM PROCESS-INSTRUCTION
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.

       PROCESS-INSTRUCTION.
           INITIALIZE WS-CMD, WS-PARAM1, WS-PARAM2.
           UNSTRING WS-INSTRUCTION DELIMITED BY SPACE
               INTO WS-CMD, WS-PARAM1, WS-PARAM2
           END-UNSTRING.

           EVALUATE TRUE
               WHEN WS-CMD = 'rect'
                   PERFORM RECTANGLE
               WHEN WS-CMD = 'rotate'
                   IF WS-PARAM1(1:4) = 'row '
                       UNSTRING WS-INSTRUCTION DELIMITED BY 'x'
                           INTO WS-CMD, WS-PARAM1, WS-PARAM2
                       END-UNSTRING
                       MOVE WS-PARAM1(5:2) TO WS-PARAM1
                       MOVE WS-PARAM2(3:2) TO WS-PARAM2
                       PERFORM ROTATE-ROW
                   ELSE IF WS-PARAM1(1:7) = 'column '
                       UNSTRING WS-INSTRUCTION DELIMITED BY 'x'
                           INTO WS-CMD, WS-PARAM1, WS-PARAM2
                       END-UNSTRING
                       MOVE WS-PARAM1(8:2) TO WS-PARAM1
                       MOVE WS-PARAM2(3:2) TO WS-PARAM2
                       PERFORM ROTATE-COLUMN
                   END-IF
           END-EVALUATE.

       RECTANGLE.
           UNSTRING WS-INSTRUCTION DELIMITED BY 'x'
               INTO WS-CMD, WS-PARAM1, WS-PARAM2
           END-UNSTRING.
           MOVE FUNCTION NUMVAL(WS-PARAM1) TO WS-PARAM1.
           MOVE FUNCTION NUMVAL(WS-PARAM2) TO WS-PARAM2.
           PERFORM VARYING WS-I FROM 0 BY 1 UNTIL WS-I >= WS-PARAM2
               PERFORM VARYING WS-J FROM 0 BY 1 UNTIL WS-J >= WS-PARAM1
                   MOVE '1' TO SCREEN-PIXEL(WS-I + 1, WS-J + 1)
               END-PERFORM
           END-PERFORM.

       ROTATE-ROW.
           MOVE FUNCTION NUMVAL(WS-PARAM1) TO WS-PARAM1.
           MOVE FUNCTION NUMVAL(WS-PARAM2) TO WS-PARAM2.
           MOVE SCREEN-ROW(WS-PARAM1 + 1) TO WS-TEMP-ROW.
           PERFORM VARYING WS-I FROM 0 BY 1 UNTIL WS-I >= SCREEN-WIDTH
               COMPUTE WS-K = (WS-I + WS-PARAM2)
               PERFORM VARYING WS-K UNTIL WS-K >= SCREEN-WIDTH
                   ADD 1 TO WS-K
               END-PERFORM
               MOVE WS-TEMP-ROW(WS-I + 1:1) TO SCREEN-PIXEL(WS-PARAM1 + 1, WS-K + 1)
           END-PERFORM.

       ROTATE-COLUMN.
           MOVE FUNCTION NUMVAL(WS-PARAM1) TO WS-PARAM1.
           MOVE FUNCTION NUMVAL(WS-PARAM2) TO WS-PARAM2.
           PERFORM VARYING WS-I FROM 0 BY 1 UNTIL WS-I >= SCREEN-HEIGHT
               MOVE SCREEN-PIXEL(WS-I + 1, WS-PARAM1 + 1) TO WS-TEMP-COL(WS-I + 1)
           END-PERFORM.
           PERFORM VARYING WS-I FROM 0 BY 1 UNTIL WS-I >= SCREEN-HEIGHT
               COMPUTE WS-K = (WS-I + WS-PARAM2)
               PERFORM VARYING WS-K UNTIL WS-K >= SCREEN-HEIGHT
                   ADD 1 TO WS-K
               END-PERFORM
               MOVE WS-TEMP-COL(WS-I + 1) TO SCREEN-PIXEL(WS-K + 1, WS-PARAM1 + 1)
           END-PERFORM.

       COUNT-LIT-PIXELS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > SCREEN-HEIGHT
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > SCREEN-WIDTH
                   IF SCREEN-PIXEL(WS-I, WS-J) = '1'
                       ADD 1 TO WS-LIT-COUNT
                   END-IF
               END-PERFORM
           END-PERFORM.
