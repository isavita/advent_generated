
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Solve.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE.
       01  IN-REC                  PIC X(1024).
       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC A(1) VALUE 'N'.
       01  WS-POS                  PIC X(1) VALUE '5'.
       01  WS-CODE                 PIC X(50) VALUE SPACES.
       01  WS-CODE-IDX             PIC 9(4) COMP VALUE 1.
       01  WS-MOVE-IDX             PIC 9(4) COMP.
       01  WS-LINE-LEN             PIC 9(4) COMP.
       01  WS-CURRENT-MOVE         PIC X(1).

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT IN-FILE.
           PERFORM UNTIL WS-EOF = 'Y'
               READ IN-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM PROCESS-LINE
               END-READ
           END-PERFORM.
           CLOSE IN-FILE.
           DISPLAY FUNCTION TRIM(WS-CODE).
           STOP RUN.

       PROCESS-LINE.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(IN-REC)) TO WS-LINE-LEN.
           PERFORM VARYING WS-MOVE-IDX FROM 1 BY 1
               UNTIL WS-MOVE-IDX > WS-LINE-LEN
               MOVE IN-REC(WS-MOVE-IDX:1) TO WS-CURRENT-MOVE
               PERFORM PROCESS-MOVE
           END-PERFORM.
           MOVE WS-POS TO WS-CODE(WS-CODE-IDX:1).
           ADD 1 TO WS-CODE-IDX.

       PROCESS-MOVE.
           EVALUATE WS-POS ALSO WS-CURRENT-MOVE
               WHEN '1' ALSO 'D' MOVE '3' TO WS-POS
               WHEN '2' ALSO 'R' MOVE '3' TO WS-POS
               WHEN '2' ALSO 'D' MOVE '6' TO WS-POS
               WHEN '3' ALSO 'U' MOVE '1' TO WS-POS
               WHEN '3' ALSO 'R' MOVE '4' TO WS-POS
               WHEN '3' ALSO 'D' MOVE '7' TO WS-POS
               WHEN '3' ALSO 'L' MOVE '2' TO WS-POS
               WHEN '4' ALSO 'L' MOVE '3' TO WS-POS
               WHEN '4' ALSO 'D' MOVE '8' TO WS-POS
               WHEN '5' ALSO 'R' MOVE '6' TO WS-POS
               WHEN '6' ALSO 'U' MOVE '2' TO WS-POS
               WHEN '6' ALSO 'R' MOVE '7' TO WS-POS
               WHEN '6' ALSO 'D' MOVE 'A' TO WS-POS
               WHEN '6' ALSO 'L' MOVE '5' TO WS-POS
               WHEN '7' ALSO 'U' MOVE '3' TO WS-POS
               WHEN '7' ALSO 'R' MOVE '8' TO WS-POS
               WHEN '7' ALSO 'D' MOVE 'B' TO WS-POS
               WHEN '7' ALSO 'L' MOVE '6' TO WS-POS
               WHEN '8' ALSO 'U' MOVE '4' TO WS-POS
               WHEN '8' ALSO 'R' MOVE '9' TO WS-POS
               WHEN '8' ALSO 'D' MOVE 'C' TO WS-POS
               WHEN '8' ALSO 'L' MOVE '7' TO WS-POS
               WHEN '9' ALSO 'L' MOVE '8' TO WS-POS
               WHEN 'A' ALSO 'U' MOVE '6' TO WS-POS
               WHEN 'A' ALSO 'R' MOVE 'B' TO WS-POS
               WHEN 'B' ALSO 'U' MOVE '7' TO WS-POS
               WHEN 'B' ALSO 'R' MOVE 'C' TO WS-POS
               WHEN 'B' ALSO 'D' MOVE 'D' TO WS-POS
               WHEN 'B' ALSO 'L' MOVE 'A' TO WS-POS
               WHEN 'C' ALSO 'U' MOVE '8' TO WS-POS
               WHEN 'C' ALSO 'L' MOVE 'B' TO WS-POS
               WHEN 'D' ALSO 'U' MOVE 'B' TO WS-POS
           END-EVALUATE.
