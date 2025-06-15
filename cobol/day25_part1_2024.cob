
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOLVE.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD           PIC X(256).
       
       WORKING-STORAGE SECTION.
       78 MAX-ITEMS              VALUE 1000.
       78 BLOCK-HEIGHT           VALUE 7.
       78 SHAPE-WIDTH            VALUE 5.
       78 SHAPE-HEIGHT           VALUE 6.
       78 MAX-FIT-HEIGHT         VALUE 5.
       
       01 WS-FILE-STATUS         PIC X(2).
       01 WS-EOF                 PIC A(1) VALUE 'N'.
          88 IS-EOF              VALUE 'Y'.
       
       01 WS-COUNTERS.
          05 WS-NUM-LOCKS         PIC 9(4) COMP VALUE 0.
          05 WS-NUM-KEYS          PIC 9(4) COMP VALUE 0.
          05 WS-TOTAL-VALID-LINES PIC 9(8) COMP VALUE 0.
          05 WS-LINES-IN-BLOCK    PIC 9(1) COMP VALUE 0.
          05 WS-MATCH-COUNT       PIC 9(9) VALUE 0.
       
       01 WS-FLAGS.
          05 WS-BLOCK-INVALID-FLAG PIC A(1) VALUE 'N'.
             88 IS-BLOCK-INVALID  VALUE 'Y'.
          05 WS-FITS-FLAG          PIC A(1).
             88 DOES-FIT          VALUE 'Y'.
       
       01 WS-CURRENT-BLOCK.
          05 WS-BLOCK-LINE        PIC X(256) OCCURS BLOCK-HEIGHT TIMES.
       
       01 WS-LOCKS-TABLE.
          05 WS-LOCK OCCURS MAX-ITEMS TIMES.
             10 WS-LOCK-COL       PIC 9(1) COMP OCCURS SHAPE-WIDTH TIMES.
       
       01 WS-KEYS-TABLE.
          05 WS-KEY OCCURS MAX-ITEMS TIMES.
             10 WS-KEY-COL        PIC 9(1) COMP OCCURS SHAPE-WIDTH TIMES.
       
       01 WS-LOOP-VARS.
          05 I                    PIC 9(4) COMP.
          05 J                    PIC 9(4) COMP.
          05 R                    PIC 9(1) COMP.
          05 C                    PIC 9(1) COMP.
          05 CNT                  PIC 9(1) COMP.
          05 EFFECTIVE-LEN        PIC 9(3) COMP.
          05 WS-REMAINDER         PIC 9(1) COMP.
          05 WS-SUM               PIC 9(2) COMP.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE.
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY 0
               STOP RUN
           END-IF.
       
           PERFORM READ-FILE-LOOP UNTIL IS-EOF.
           CLOSE INPUT-FILE.
       
           IF WS-TOTAL-VALID-LINES = 0
               DISPLAY 0
               STOP RUN
           END-IF.
       
           DIVIDE WS-TOTAL-VALID-LINES BY BLOCK-HEIGHT
               GIVING I REMAINDER WS-REMAINDER.
           IF WS-REMAINDER NOT = 0
               DISPLAY 0
               STOP RUN
           END-IF.
       
           PERFORM CALCULATE-MATCHES.
           DISPLAY FUNCTION TRIM(WS-MATCH-COUNT).
           STOP RUN.
       
       READ-FILE-LOOP.
           READ INPUT-FILE AT END SET IS-EOF TO TRUE.
           IF NOT IS-EOF
               PERFORM PROCESS-LINE
           END-IF.
       
       PROCESS-LINE.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(INPUT-RECORD, TRAILING))
               TO EFFECTIVE-LEN.
           IF EFFECTIVE-LEN > 0
               ADD 1 TO WS-TOTAL-VALID-LINES, WS-LINES-IN-BLOCK
               MOVE INPUT-RECORD TO WS-BLOCK-LINE(WS-LINES-IN-BLOCK)
               IF EFFECTIVE-LEN < SHAPE-WIDTH
                   SET IS-BLOCK-INVALID TO TRUE
               END-IF
               IF WS-LINES-IN-BLOCK = BLOCK-HEIGHT
                   PERFORM PROCESS-BLOCK
                   MOVE 0 TO WS-LINES-IN-BLOCK
                   MOVE 'N' TO WS-BLOCK-INVALID-FLAG
               END-IF
           END-IF.
       
       PROCESS-BLOCK.
           IF NOT IS-BLOCK-INVALID
               IF WS-BLOCK-LINE(1)(1:SHAPE-WIDTH) = "#####"
                   ADD 1 TO WS-NUM-LOCKS
                   PERFORM PARSE-LOCK
               ELSE
                   ADD 1 TO WS-NUM-KEYS
                   PERFORM PARSE-KEY
               END-IF
           END-IF.
       
       PARSE-LOCK.
           PERFORM VARYING C FROM 1 BY 1 UNTIL C > SHAPE-WIDTH
               MOVE 0 TO CNT
               PERFORM VARYING R FROM 2 BY 1 UNTIL R > BLOCK-HEIGHT
                   IF WS-BLOCK-LINE(R)(C:1) = '#'
                       ADD 1 TO CNT
                   ELSE
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               MOVE CNT TO WS-LOCK-COL(WS-NUM-LOCKS, C)
           END-PERFORM.
       
       PARSE-KEY.
           PERFORM VARYING C FROM 1 BY 1 UNTIL C > SHAPE-WIDTH
               MOVE 0 TO CNT
               PERFORM VARYING R FROM SHAPE-HEIGHT BY -1 UNTIL R < 1
                   IF WS-BLOCK-LINE(R)(C:1) = '#'
                       ADD 1 TO CNT
                   ELSE
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               MOVE CNT TO WS-KEY-COL(WS-NUM-KEYS, C)
           END-PERFORM.
       
       CALCULATE-MATCHES.
           MOVE 0 TO WS-MATCH-COUNT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-NUM-LOCKS
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-NUM-KEYS
                   PERFORM CHECK-FIT
                   IF DOES-FIT
                       ADD 1 TO WS-MATCH-COUNT
                   END-IF
               END-PERFORM
           END-PERFORM.
       
       CHECK-FIT.
           MOVE 'Y' TO WS-FITS-FLAG.
           PERFORM VARYING C FROM 1 BY 1 UNTIL C > SHAPE-WIDTH
               COMPUTE WS-SUM = WS-LOCK-COL(I, C) + WS-KEY-COL(J, C)
               IF WS-SUM > MAX-FIT-HEIGHT
                   MOVE 'N' TO WS-FITS-FLAG
                   EXIT PERFORM
               END-IF
           END-PERFORM.
