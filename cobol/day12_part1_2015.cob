
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SumFileNumbers.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD           PIC X(4096).

       WORKING-STORAGE SECTION.
       01  WS-INPUT-LINE          PIC X(4096).
       01  WS-EOF-FLAG            PIC X VALUE 'N'.
           88 WS-EOF              VALUE 'Y'.
       01  WS-LINE-LEN            PIC 9(4) COMP.
       01  WS-IDX                 PIC 9(4) COMP.
       01  WS-CURRENT-NUM         PIC S9(10) COMP VALUE 0.
       01  WS-TOTAL-SUM           PIC S9(18) COMP VALUE 0.
       01  WS-IS-NEGATIVE-FLAG    PIC X VALUE 'N'.
           88 WS-IS-NEGATIVE      VALUE 'Y'.
       01  WS-DISPLAY-SUM         PIC -Z(17)9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL WS-EOF
               READ INPUT-FILE INTO WS-INPUT-LINE
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       PERFORM PROCESS-LINE
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.

           PERFORM ADD-PENDING-NUMBER.

           MOVE WS-TOTAL-SUM TO WS-DISPLAY-SUM.
           DISPLAY FUNCTION TRIM(WS-DISPLAY-SUM).
           STOP RUN.

       PROCESS-LINE.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-INPUT-LINE))
             TO WS-LINE-LEN.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-LINE-LEN
                   EVALUATE WS-INPUT-LINE(WS-IDX:1)
                       WHEN '-'
                           SET WS-IS-NEGATIVE TO TRUE
                       WHEN '0' THROUGH '9'
                           COMPUTE WS-CURRENT-NUM =
                               (WS-CURRENT-NUM * 10) +
                               FUNCTION NUMVAL(WS-INPUT-LINE(WS-IDX:1))
                       WHEN OTHER
                           PERFORM ADD-PENDING-NUMBER
                   END-EVALUATE
           END-PERFORM.
           PERFORM ADD-PENDING-NUMBER.

       ADD-PENDING-NUMBER.
           IF WS-IS-NEGATIVE
               COMPUTE WS-CURRENT-NUM = WS-CURRENT-NUM * -1
           END-IF.
           ADD WS-CURRENT-NUM TO WS-TOTAL-SUM.
           MOVE 0 TO WS-CURRENT-NUM.
           MOVE 'N' TO WS-IS-NEGATIVE-FLAG.
