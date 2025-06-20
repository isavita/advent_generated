```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAND-FILLING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'input.txt'.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  LINE-DATA         PIC X(100).

       WORKING-STORAGE SECTION.
       01  GRID-DATA.
           05  GRID-CELL OCCURS 1000 TIMES DEPENDING ON MAX-X.
               10  ROW OCCURS 1000 TIMES DEPENDING ON MAX-Y.
                   15  CELL-OCCUPIED   PIC X VALUE SPACE.

       01  MAX-X               PIC 9(4) VALUE 0.
       01  MAX-Y               PIC 9(4) VALUE 0.
       01  CURRENT-X           PIC 9(4).
       01  CURRENT-Y           PIC 9(4).
       01  X1                  PIC 9(4).
       01  Y1                  PIC 9(4).
       01  X2                  PIC 9(4).
       01  Y2                  PIC 9(4).
       01  I                   PIC 9(4).
       01  J                   PIC 9(4).
       01  MIN-X               PIC 9(4) VALUE 9999.
       01  MIN-Y               PIC 9(4) VALUE 9999.
       01  MAX-X-BOUND         PIC 9(4) VALUE 0.
       01  MAX-Y-BOUND         PIC 9(4) VALUE 0.

       01  SAND-COUNT          PIC 9(9) VALUE 0.
       01  FIRST-FLOOR-TOUCH   PIC 9(9) VALUE 0.
       01  SAND-X              PIC 9(4) VALUE 500.
       01  SAND-Y              PIC 9(4) VALUE 0.
       01  SETTLED             PIC X VALUE 'N'.
       01  FULL                PIC X VALUE 'N'.
       01  NEXT-SAND-X         PIC 9(4).
       01  NEXT-SAND-Y         PIC 9(4).
       01  DX                  PIC S9(1).
       01  DY                  PIC S9(1).
       01  LOOP-COUNTER        PIC 9(4).

       01  PARSING-VARS.
           05  POINT-STR       PIC X(20).
           05  COMMA-POS       PIC 9(2).
           05  ARROW-POS       PIC 9(2).
           05  POINT-DATA      PIC X(100).
           05  POINT-PTR       PIC 9(2) VALUE 1.
           05  POINT-LEN       PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-GRID.
           PERFORM READ-INPUT-FILE.
           PERFORM CALCULATE-BOUNDS.
           PERFORM FILL-SAND.
           DISPLAY FIRST-FLOOR-TOUCH.
           STOP RUN.

       INITIALIZE-GRID.
           MOVE 0 TO MAX-X, MAX-Y.
           MOVE 0 TO CELL-OCCUPIED.

       READ-INPUT-FILE.
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL EOF
               READ INPUT-FILE
                   AT END MOVE 'Y' TO EOF
                   NOT AT END
                       MOVE LINE-DATA TO POINT-DATA
                       PERFORM PARSE-LINE
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.

       PARSE-LINE.
           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ' ' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO POINT-PTR
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA)
               IF POINT-DATA(POINT-PTR:1) = ','
                   MOVE POINT-PTR TO COMMA-POS
               ELSE IF POINT-DATA(POINT-PTR:1) = '-' AND POINT-DATA(POINT-PTR+1:1) = '>'
                   MOVE POINT-PTR TO ARROW-POS
               END-IF
               ADD 1 TO POINT-PTR
           END-PERFORM.

           MOVE 1 TO POINT-PTR.
           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL POINT-PTR > FUNCTION LENGTH(POINT-DATA