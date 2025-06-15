
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Solve.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD         PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-EOF               PIC A(1) VALUE 'N'.
           88 FILE-AT-END                VALUE 'Y'.

       01  WS-MSG-LEN           PIC 9(3) VALUE 0.
       01  WS-FIRST-LINE-FLAG   PIC A(1) VALUE 'Y'.

       01  WS-RESULT            PIC X(100).

       01  WS-COUNTERS.
           05 I                 PIC 9(3).
           05 J                 PIC 9(3).

       01  WS-MAX-FINDER.
           05 WS-MAX-COUNT      PIC 9(4).
           05 WS-MAX-CHAR-ASCII PIC 9(3).

       01  WS-COUNT-TABLE.
           05 WS-POSITION OCCURS 100 TIMES.
              10 WS-CHAR-COUNT  PIC 9(4) OCCURS 128 TIMES.

       PROCEDURE DIVISION.
       MAIN.
           INITIALIZE WS-COUNT-TABLE.
           OPEN INPUT INPUT-FILE.

           PERFORM UNTIL FILE-AT-END
               READ INPUT-FILE
                   AT END SET FILE-AT-END TO TRUE
                   NOT AT END PERFORM PROCESS-LINE
               END-READ
           END-PERFORM.

           CLOSE INPUT-FILE.

           PERFORM BUILD-RESULT.

           IF WS-MSG-LEN > 0
              DISPLAY FUNCTION TRIM(WS-RESULT)
           END-IF.

           STOP RUN.

       PROCESS-LINE.
           IF WS-FIRST-LINE-FLAG = 'Y'
              COMPUTE WS-MSG-LEN =
                 FUNCTION STORED-CHAR-LENGTH(INPUT-RECORD)
              MOVE 'N' TO WS-FIRST-LINE-FLAG
           END-IF.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-MSG-LEN
               COMPUTE J = FUNCTION ORD(INPUT-RECORD(I:1)) + 1
               ADD 1 TO WS-CHAR-COUNT(I, J)
           END-PERFORM.

       BUILD-RESULT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-MSG-LEN
               MOVE 0 TO WS-MAX-COUNT
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 128
                   IF WS-CHAR-COUNT(I, J) > WS-MAX-COUNT
                       MOVE WS-CHAR-COUNT(I, J) TO WS-MAX-COUNT
                       MOVE J TO WS-MAX-CHAR-ASCII
                   END-IF
               END-PERFORM
               MOVE FUNCTION CHAR(WS-MAX-CHAR-ASCII - 1)
                   TO WS-RESULT(I:1)
           END-PERFORM.
