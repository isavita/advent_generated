
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROCK-PAPER-SCISSORS.
       AUTHOR. EXPERT PROGRAMMER.

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
       01 INPUT-RECORD.
           05 OPPONENT-MOVE PIC X.
           05 FILLER PIC X.
           05 YOUR-MOVE PIC X.
           05 FILLER-NL PIC X.

       WORKING-STORAGE SECTION.
       01 WS-TOTAL-SCORE PIC 9(9) VALUE 0.
       01 WS-SCORE PIC 9(2).
       01 WS-FILE-STATUS PIC XX VALUE '00'.
           88 FILE-OK VALUE '00'.
           88 END-OF-FILE VALUE '10'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE.
           IF NOT FILE-OK
               DISPLAY 'Error opening file'
               GOBACK
           END-IF.

           PERFORM READ-AND-PROCESS-LINE UNTIL END-OF-FILE.

           CLOSE INPUT-FILE.
           DISPLAY WS-TOTAL-SCORE.

           GOBACK.

       READ-AND-PROCESS-LINE.
           READ INPUT-FILE
               AT END MOVE '10' TO WS-FILE-STATUS
           END-READ.

           IF FILE-OK
               MOVE 0 TO WS-SCORE
               EVALUATE YOUR-MOVE
                   WHEN 'X'
                       ADD 1 TO WS-SCORE
                   WHEN 'Y'
                       ADD 2 TO WS-SCORE
                   WHEN 'Z'
                       ADD 3 TO WS-SCORE
               END-EVALUATE

               EVALUATE TRUE
                   WHEN (OPPONENT-MOVE = 'A' AND YOUR-MOVE = 'Y') OR
                        (OPPONENT-MOVE = 'B' AND YOUR-MOVE = 'Z') OR
                        (OPPONENT-MOVE = 'C' AND YOUR-MOVE = 'X')
                       ADD 6 TO WS-SCORE
                   WHEN (OPPONENT-MOVE = 'A' AND YOUR-MOVE = 'X') OR
                        (OPPONENT-MOVE = 'B' AND YOUR-MOVE = 'Y') OR
                        (OPPONENT-MOVE = 'C' AND YOUR-MOVE = 'Z')
                       ADD 3 TO WS-SCORE
               END-EVALUATE

               ADD WS-SCORE TO WS-TOTAL-SCORE
           END-IF.
