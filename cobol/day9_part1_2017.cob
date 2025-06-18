
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GARBAGECOLLECTOR.
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
       01 INPUT-RECORD PIC X.

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS      PIC XX VALUE '00'.
          88 FILE-OK          VALUE '00'.
       01 WS-SCORE           PIC 9(9) VALUE 0.
       01 WS-DEPTH           PIC 9(9) VALUE 0.
       01 WS-IN-GARBAGE      PIC X  VALUE 'N'.
          88 GARBAGE-MODE     VALUE 'Y'.
       01 WS-IGNORE-NEXT     PIC X  VALUE 'N'.
          88 IGNORE-CHAR      VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE.
           IF NOT FILE-OK
               DISPLAY 'Error opening input.txt'
               GOBACK
           END-IF.

           PERFORM PROCESS-FILE UNTIL WS-FILE-STATUS NOT = '00'.

           CLOSE INPUT-FILE.

           DISPLAY WS-SCORE.

           GOBACK.

       PROCESS-FILE.
           READ INPUT-FILE
               AT END MOVE '10' TO WS-FILE-STATUS
               NOT AT END PERFORM PROCESS-CHARACTER
           END-READ.

       PROCESS-CHARACTER.
           IF IGNORE-CHAR
               MOVE 'N' TO WS-IGNORE-NEXT
               EXIT PARAGRAPH
           END-IF.

           IF GARBAGE-MODE
               EVALUATE INPUT-RECORD
                   WHEN '!'
                       MOVE 'Y' TO WS-IGNORE-NEXT
                   WHEN '>'
                       MOVE 'N' TO WS-IN-GARBAGE
               END-EVALUATE
           ELSE
               EVALUATE INPUT-RECORD
                   WHEN '{'
                       ADD 1 TO WS-DEPTH
                   WHEN '}'
                       ADD WS-DEPTH TO WS-SCORE
                       SUBTRACT 1 FROM WS-DEPTH
                   WHEN '<'
                       MOVE 'Y' TO WS-IN-GARBAGE
               END-EVALUATE
           END-IF.
