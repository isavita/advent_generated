
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Solve.
       AUTHOR. Expert Programmer.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD           PIC X(1000).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS         PIC XX.
       01  WS-EOF-FLAG            PIC X VALUE 'N'.
           88 IS-EOF             VALUE 'Y'.

       01  WS-COUNTERS.
           05 WS-MESSAGE-COUNT    PIC 9(4) VALUE 0.
           05 WS-MESSAGE-LENGTH   PIC 9(4) VALUE 0.
           05 I                   PIC 9(4).
           05 J                   PIC 9(4).
           05 K                   PIC 9(4).

       01  WS-MESSAGES-TABLE.
           05 WS-MESSAGE          PIC X(1000)
              OCCURS 1000 TIMES.

       01  WS-FREQ-TABLE.
           05 WS-FREQ-ENTRY       OCCURS 256 TIMES.
              10 WS-FREQ-COUNT    PIC 9(4).

       01  WS-MIN-FINDER.
           05 WS-MIN-COUNT        PIC 9(4).
           05 WS-MIN-CHAR         PIC X.
           05 WS-ORD-CHAR         PIC 9(3).

       01  WS-RESULT-MESSAGE      PIC X(1000).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE.
           PERFORM READ-ALL-MESSAGES UNTIL IS-EOF.
           CLOSE INPUT-FILE.

           IF WS-MESSAGE-COUNT = 0
               DISPLAY SPACES
               STOP RUN
           END-IF.

           COMPUTE WS-MESSAGE-LENGTH =
               FUNCTION LENGTH(FUNCTION TRIM(WS-MESSAGE(1))).

           PERFORM PROCESS-EACH-COLUMN
               VARYING J FROM 1 BY 1
               UNTIL J > WS-MESSAGE-LENGTH.

           DISPLAY FUNCTION TRIM(WS-RESULT-MESSAGE).
           STOP RUN.

       READ-ALL-MESSAGES.
           READ INPUT-FILE INTO INPUT-RECORD
               AT END MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   ADD 1 TO WS-MESSAGE-COUNT
                   MOVE INPUT-RECORD TO WS-MESSAGE(WS-MESSAGE-COUNT)
           END-READ.

       PROCESS-EACH-COLUMN.
           INITIALIZE WS-FREQ-TABLE.

           PERFORM COUNT-CHARS-IN-COLUMN
               VARYING I FROM 1 BY 1
               UNTIL I > WS-MESSAGE-COUNT.

           PERFORM FIND-LEAST-COMMON-CHAR.

           MOVE WS-MIN-CHAR TO WS-RESULT-MESSAGE(J:1).

       COUNT-CHARS-IN-COLUMN.
           COMPUTE WS-ORD-CHAR = FUNCTION ORD(WS-MESSAGE(I)(J:1)) + 1.
           ADD 1 TO WS-FREQ-COUNT(WS-ORD-CHAR).

       FIND-LEAST-COMMON-CHAR.
           MOVE 9999 TO WS-MIN-COUNT.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 256
               IF WS-FREQ-COUNT(K) > 0 AND
                  WS-FREQ-COUNT(K) < WS-MIN-COUNT
                   MOVE WS-FREQ-COUNT(K) TO WS-MIN-COUNT
                   MOVE FUNCTION CHAR(K - 1) TO WS-MIN-CHAR
               END-IF
           END-PERFORM.
