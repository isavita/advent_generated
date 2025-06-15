
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
       01  IN-REC           PIC X(256).

       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           05 WS-EOF-FLAG    PIC X VALUE 'N'.
              88 IS-EOF      VALUE 'Y'.
           05 WS-IN-BRACKETS PIC X.
              88 IN-BRACKETS VALUE 'Y'.
           05 WS-MATCH-FOUND PIC X.
              88 MATCH-FOUND VALUE 'Y'.

       01  WS-VARS.
           05 WS-SSL-COUNT   PIC 9(5) VALUE 0.
           05 WS-LINE-LEN    PIC 9(4).
           05 WS-IDX         PIC 9(4).

       01  WS-SUPERNET-LIST.
           05 WS-SUPER-COUNT PIC 9(3).
           05 WS-SUPER-PAIRS OCCURS 50 TIMES INDEXED BY SUPER-IDX.
              10 WS-SUPER-A  PIC X.
              10 WS-SUPER-B  PIC X.

       01  WS-HYPERNET-LIST.
           05 WS-HYPER-COUNT PIC 9(3).
           05 WS-HYPER-PAIRS OCCURS 50 TIMES INDEXED BY HYPER-IDX.
              10 WS-HYPER-A  PIC X.
              10 WS-HYPER-B  PIC X.

       PROCEDURE DIVISION.
       main.
           OPEN INPUT IN-FILE.
           PERFORM UNTIL IS-EOF
               READ IN-FILE INTO IN-REC
                   AT END SET IS-EOF TO TRUE
                   NOT AT END PERFORM process-line
               END-READ
           END-PERFORM.
           CLOSE IN-FILE.
           DISPLAY WS-SSL-COUNT.
           STOP RUN.

       process-line.
           PERFORM initialize-for-line.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(IN-REC)) TO WS-LINE-LEN.

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-LINE-LEN
               EVALUATE IN-REC(WS-IDX:1)
                   WHEN '[' SET IN-BRACKETS TO TRUE
                   WHEN ']' MOVE 'N' TO WS-IN-BRACKETS
                   WHEN OTHER
                       IF WS-IDX <= WS-LINE-LEN - 2
                           PERFORM find-patterns
                       END-IF
               END-EVALUATE
           END-PERFORM.

           PERFORM compare-lists.

       initialize-for-line.
           MOVE 0 TO WS-SUPER-COUNT, WS-HYPER-COUNT.
           MOVE 'N' TO WS-IN-BRACKETS.
           MOVE 'N' TO WS-MATCH-FOUND.

       find-patterns.
           IF IN-REC(WS-IDX:1) = IN-REC(WS-IDX + 2:1) AND
              IN-REC(WS-IDX:1) NOT = IN-REC(WS-IDX + 1:1)
               IF IN-BRACKETS
                   ADD 1 TO WS-HYPER-COUNT
                   IF WS-HYPER-COUNT <= 50
                       MOVE IN-REC(WS-IDX + 1:1) TO
                           WS-HYPER-A(WS-HYPER-COUNT)
                       MOVE IN-REC(WS-IDX:1) TO
                           WS-HYPER-B(WS-HYPER-COUNT)
                   END-IF
               ELSE
                   ADD 1 TO WS-SUPER-COUNT
                   IF WS-SUPER-COUNT <= 50
                       MOVE IN-REC(WS-IDX:1) TO
                           WS-SUPER-A(WS-SUPER-COUNT)
                       MOVE IN-REC(WS-IDX + 1:1) TO
                           WS-SUPER-B(WS-SUPER-COUNT)
                   END-IF
               END-IF
           END-IF.

       compare-lists.
           PERFORM VARYING SUPER-IDX FROM 1 BY 1
               UNTIL SUPER-IDX > WS-SUPER-COUNT OR MATCH-FOUND
               PERFORM VARYING HYPER-IDX FROM 1 BY 1
                   UNTIL HYPER-IDX > WS-HYPER-COUNT OR MATCH-FOUND
                   IF WS-SUPER-A(SUPER-IDX) = WS-HYPER-A(HYPER-IDX) AND
                      WS-SUPER-B(SUPER-IDX) = WS-HYPER-B(HYPER-IDX)
                       SET MATCH-FOUND TO TRUE
                   END-IF
               END-PERFORM
           END-PERFORM.
           IF MATCH-FOUND
               ADD 1 TO WS-SSL-COUNT
           END-IF.
