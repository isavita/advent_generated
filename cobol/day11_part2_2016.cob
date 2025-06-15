
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Day11.
       AUTHOR. AI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-WORK-FILE ASSIGN TO "sortwork.tmp".

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD         PIC X(256).

       SD SORT-WORK-FILE.
       01 SORT-REC.
           05 S-GEN-FLOOR      PIC 9(2).
           05 S-CHIP-FLOOR     PIC 9(2).

       WORKING-STORAGE SECTION.
       01 CONSTANTS.
           05 MAX-MATERIALS    PIC 9(2) VALUE 14.
           05 NUM-FLOORS       PIC 9(1) VALUE 4.
           05 QUEUE-SIZE       PIC 9(7) VALUE 2000000.
           05 HASH-SIZE        PIC 9(7) VALUE 4000037.
           05 MAX-ITEMS        PIC 9(2) VALUE 28.

       01 FILE-VARS.
           05 WS-EOF-FLAG      PIC X VALUE 'N'.
              88 IS-EOF        VALUE 'Y'.
           05 WS-LINE          PIC X(256).

       01 PARSE-VARS.
           05 WS-WORD-COUNT    PIC 9(2).
           05 WS-WORDS         OCCURS 32 TIMES PIC X(20).

       01 MATERIALS-DATA.
           05 NUM-MATERIALS-VAL PIC 9(2) COMP-5 VALUE 0.
           05 MATERIALS-TABLE  OCCURS 14 TIMES.
               10 MAT-NAME     PIC X(20).

       01 STATE-REC.
           05 FLOORS           OCCURS 4 TIMES PIC 9(9) COMP-5.
           05 ELEVATOR         PIC 9(2) COMP-5.
           05 STEPS            PIC 9(4) COMP-5.

       01 INITIAL-STATE        LIKE STATE-REC.
       01 CURRENT-STATE        LIKE STATE-REC.
       01 NEXT-STATE           LIKE STATE-REC.

       01 QUEUE-DATA.
           05 Q-FRONT          PIC 9(7) COMP-5 VALUE 1.
           05 Q-REAR           PIC 9(7) COMP-5 VALUE 1.
           05 QUEUE-TABLE.
               10 QUEUE-ITEM   OCCURS 2000000 TIMES LIKE STATE-REC.

       01 HASH-DATA.
           05 NODE-IDX         PIC 9(7) COMP-5 VALUE 0.
           05 HASH-TABLE       OCCURS 4000037 TIMES PIC 9(7) COMP-5.
           05 NODE-POOL-TABLE.
               10 NODE-POOL    OCCURS 2000000 TIMES.
                   15 POOL-KEY     PIC S9(18) COMP-5.
                   15 POOL-NEXT    PIC 9(7) COMP-5.

       01 PAIR-DATA.
           05 PAIR-TABLE.
               10 PAIR-ITEM    OCCURS 14 TIMES.
                   15 P-GEN-FLOOR  PIC 9(2).
                   15 P-CHIP-FLOOR PIC 9(2).
           05 WS-KEY           PIC S9(18) COMP-5.

       01 WS-VARS.
           05 I                PIC 9(4) COMP-5.
           05 J                PIC 9(4) COMP-5.
           05 F                PIC 9(2) COMP-5.
           05 D                PIC S9(2) COMP-5.
           05 MAT-ID           PIC 9(2) COMP-5.
           05 TEMP-FLOOR       PIC 9(9) COMP-5.
           05 MASK-VAL         PIC 9(9) COMP-5.
           05 MASK-VAL-2       PIC 9(9) COMP-5.
           05 FINAL-MASK       PIC 9(9) COMP-5.
           05 NEXT-ELEV        PIC 9(2) COMP-5.
           05 HASH-IDX         PIC 9(7) COMP-5.
           05 CURR-NODE-IDX    PIC 9(7) COMP-5.
           05 IS-VISITED-FLAG  PIC X.
           05 IS-VALID-FLAG    PIC X.
           05 ITEMS-ON-FLOOR-COUNT PIC 9(2) COMP-5.
           05 ITEMS-ON-FLOOR   OCCURS 28 TIMES PIC 9(2) COMP-5.
           05 POWER-OF-2       OCCURS 29 TIMES PIC 9(9) COMP-5.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM SOLVE.
           STOP RUN.

       SOLVE.
           PERFORM INITIALIZE-ALL.
           PERFORM READ-AND-PARSE-INPUT.
           PERFORM ADD-PART2-ITEMS.
           PERFORM CALCULATE-FINAL-MASK.

           MOVE INITIAL-STATE TO QUEUE-ITEM(Q-REAR).
           ADD 1 TO Q-REAR.
           PERFORM GET-KEY USING INITIAL-STATE GIVING WS-KEY.
           PERFORM IS-VISITED USING WS-KEY GIVING IS-VISITED-FLAG.

           PERFORM UNTIL Q-FRONT >= Q-REAR
               MOVE QUEUE-ITEM(Q-FRONT) TO CURRENT-STATE
               ADD 1 TO Q-FRONT

               IF FLOORS OF CURRENT-STATE(NUM-FLOORS) = FINAL-MASK
                   DISPLAY STEPS OF CURRENT-STATE
                   STOP RUN
               END-IF
               PERFORM GENERATE-MOVES
           END-PERFORM.

       INITIALIZE-ALL.
           INITIALIZE QUEUE-DATA, HASH-DATA, MATERIALS-DATA.
           INITIALIZE INITIAL-STATE.
           MOVE 1 TO POWER-OF-2(1).
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 29
               COMPUTE POWER-OF-2(I) = POWER-OF-2(I - 1) * 2
           END-PERFORM.

       READ-AND-PARSE-INPUT.
           OPEN INPUT INPUT-FILE.
           PERFORM VARYING F FROM 1 BY 1 UNTIL F > NUM-FLOORS
               READ INPUT-FILE INTO WS-LINE
                   AT END SET IS-EOF TO TRUE
               END-READ
               IF NOT IS-EOF
                   PERFORM PARSE-LINE
               END-IF
           END-PERFORM.
           CLOSE INPUT-FILE.

       PARSE-LINE.
           INSPECT WS-LINE REPLACING ALL "," BY " ".
           INSPECT WS-LINE REPLACING ALL "." BY " ".
           INSPECT WS-LINE REPLACING ALL "-" BY " ".
           UNSTRING WS-LINE DELIMITED BY ALL " "
               INTO WS-WORDS(1), WS-WORDS(2), WS-WORDS(3),
                    WS-WORDS(4), WS-WORDS(5), WS-WORDS(6),
                    WS-WORDS(7), WS-WORDS(8), WS-WORDS(9),
                    WS-WORDS(10), WS-WORDS(11), WS-WORDS(12),
                    WS-WORDS(13), WS-WORDS(14), WS-WORDS(15),
                    WS-WORDS(16), WS-WORDS(17), WS-WORDS(18),
                    WS-WORDS(19), WS-WORDS(20), WS-WORDS(21),
                    WS-WORDS(22), WS-WORDS(23), WS-WORDS(24)
               COUNT IN WS-WORD-COUNT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-WORD-COUNT
               IF WS-WORDS(I) = "generator"
                   PERFORM GET-MAT-ID USING WS-WORDS(I - 1)
                       GIVING MAT-ID
                   COMPUTE MASK-VAL = POWER-OF-2(MAT-ID)
                   ADD MASK-VAL TO FLOORS OF INITIAL-STATE(F)
               ELSE IF WS-WORDS(I) = "microchip"
                   PERFORM GET-MAT-ID USING WS-WORDS(I - 2)
                       GIVING MAT-ID
                   COMPUTE MASK-VAL = POWER-OF-2(MAT-ID + MAX-MATERIALS)
                   ADD MASK-VAL TO FLOORS OF INITIAL-STATE(F)
               END-IF
           END-PERFORM.

       GET-MAT-ID USING IN-MAT-NAME, GIVING OUT-MAT-ID.
           MOVE 0 TO OUT-MAT-ID.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-MATERIALS-VAL
               IF MAT-NAME(I) = IN-MAT-NAME
                   MOVE I TO OUT-MAT-ID
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           IF OUT-MAT-ID = 0
               ADD 1 TO NUM-MATERIALS-VAL
               MOVE IN-MAT-NAME TO MAT-NAME(NUM-MATERIALS-VAL)
               MOVE NUM-MATERIALS-VAL TO OUT-MAT-ID
           END-IF.

       ADD-PART2-ITEMS.
           PERFORM GET-MAT-ID USING "elerium" GIVING MAT-ID.
           COMPUTE MASK-VAL = POWER-OF-2(MAT-ID).
           ADD MASK-VAL TO FLOORS OF INITIAL-STATE(1).
           COMPUTE MASK-VAL = POWER-OF-2(MAT-ID + MAX-MATERIALS).
           ADD MASK-VAL TO FLOORS OF INITIAL-STATE(1).
           PERFORM GET-MAT-ID USING "dilithium" GIVING MAT-ID.
           COMPUTE MASK-VAL = POWER-OF-2(MAT-ID).
           ADD MASK-VAL TO FLOORS OF INITIAL-STATE(1).
           COMPUTE MASK-VAL = POWER-OF-2(MAT-ID + MAX-MATERIALS).
           ADD MASK-VAL TO FLOORS OF INITIAL-STATE(1).

       CALCULATE-FINAL-MASK.
           MOVE 0 TO FINAL-MASK.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-MATERIALS-VAL
               ADD POWER-OF-2(I) TO FINAL-MASK
               ADD POWER-OF-2(I + MAX-MATERIALS) TO FINAL-MASK
           END-PERFORM.

       GENERATE-MOVES.
           MOVE 0 TO ITEMS-ON-FLOOR-COUNT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-ITEMS * 2
               COMPUTE MASK-VAL = POWER-OF-2(I)
               IF FUNCTION B-AND(FLOORS OF CURRENT-STATE(ELEVATOR OF
                   CURRENT-STATE), MASK-VAL) > 0
                   ADD 1 TO ITEMS-ON-FLOOR-COUNT
                   MOVE I TO ITEMS-ON-FLOOR(ITEMS-ON-FLOOR-COUNT)
               END-IF
           END-PERFORM.

           PERFORM VARYING D FROM -1 BY 2 UNTIL D > 1
               COMPUTE NEXT-ELEV = ELEVATOR OF CURRENT-STATE + D
               IF NEXT-ELEV > 0 AND NEXT-ELEV <= NUM-FLOORS
                   PERFORM GENERATE-ONE-ITEM-MOVES
                   PERFORM GENERATE-TWO-ITEM-MOVES
               END-IF
           END-PERFORM.

       GENERATE-ONE-ITEM-MOVES.
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ITEMS-ON-FLOOR-COUNT
               MOVE CURRENT-STATE TO NEXT-STATE
               ADD 1 TO STEPS OF NEXT-STATE
               MOVE NEXT-ELEV TO ELEVATOR OF NEXT-STATE
               MOVE POWER-OF-2(ITEMS-ON-FLOOR(I)) TO MASK-VAL

               SUBTRACT MASK-VAL FROM FLOORS OF NEXT-STATE(
                   ELEVATOR OF CURRENT-STATE).
               ADD MASK-VAL TO FLOORS OF NEXT-STATE(NEXT-ELEV).
               PERFORM CHECK-AND-ENQUEUE
           END-PERFORM.

       GENERATE-TWO-ITEM-MOVES.
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ITEMS-ON-FLOOR-COUNT - 1
               PERFORM VARYING J FROM I + 1 BY 1
                   UNTIL J > ITEMS-ON-FLOOR-COUNT
                   MOVE CURRENT-STATE TO NEXT-STATE
                   ADD 1 TO STEPS OF NEXT-STATE
                   MOVE NEXT-ELEV TO ELEVATOR OF NEXT-STATE
                   MOVE POWER-OF-2(ITEMS-ON-FLOOR(I)) TO MASK-VAL
                   MOVE POWER-OF-2(ITEMS-ON-FLOOR(J)) TO MASK-VAL-2
                   ADD MASK-VAL TO MASK-VAL-2

                   SUBTRACT MASK-VAL-2 FROM FLOORS OF NEXT-STATE(
                       ELEVATOR OF CURRENT-STATE).
                   ADD MASK-VAL-2 TO FLOORS OF NEXT-STATE(NEXT-ELEV).
                   PERFORM CHECK-AND-ENQUEUE
               END-PERFORM
           END-PERFORM.

       CHECK-AND-ENQUEUE.
           PERFORM IS-FLOOR-VALID
               USING FLOORS OF NEXT-STATE(NEXT-ELEV)
               GIVING IS-VALID-FLAG.
           IF IS-VALID-FLAG = 'Y'
               PERFORM IS-FLOOR-VALID
                   USING FLOORS OF NEXT-STATE(ELEVATOR OF CURRENT-STATE)
                   GIVING IS-VALID-FLAG
               IF IS-VALID-FLAG = 'Y'
                   PERFORM GET-KEY USING NEXT-STATE GIVING WS-KEY
                   PERFORM IS-VISITED USING WS-KEY
                       GIVING IS-VISITED-FLAG
                   IF IS-VISITED-FLAG = 'N'
                       MOVE NEXT-STATE TO QUEUE-ITEM(Q-REAR)
                       ADD 1 TO Q-REAR
                   END-IF
               END-IF
           END-IF.

       IS-FLOOR-VALID USING IN-FLOOR-MASK, GIVING OUT-IS-VALID-FLAG.
           MOVE 'Y' TO OUT-IS-VALID-FLAG.
           COMPUTE MASK-VAL = POWER-OF-2(MAX-MATERIALS + 1) - 1.
           COMPUTE I = FUNCTION B-AND(IN-FLOOR-MASK, MASK-VAL).
           IF I = 0
               EXIT PARAGRAPH
           END-IF.
           COMPUTE J = FUNCTION QUOTIENT(IN-FLOOR-MASK,
               POWER-OF-2(MAX-MATERIALS + 1)).
           COMPUTE I = FUNCTION B-AND(J, FUNCTION B-NOT(I)).
           IF I > 0
               MOVE 'N' TO OUT-IS-VALID-FLAG
           END-IF.

       GET-KEY USING IN-STATE, GIVING OUT-KEY.
           INITIALIZE PAIR-TABLE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-MATERIALS-VAL
               PERFORM VARYING F FROM 1 BY 1 UNTIL F > NUM-FLOORS
                   IF FUNCTION B-AND(FLOORS OF IN-STATE(F),
                       POWER-OF-2(I)) > 0
                       MOVE F TO P-GEN-FLOOR(I)
                   END-IF
                   IF FUNCTION B-AND(FLOORS OF IN-STATE(F),
                       POWER-OF-2(I + MAX-MATERIALS)) > 0
                       MOVE F TO P-CHIP-FLOOR(I)
                   END-IF
               END-PERFORM
           END-PERFORM.

           SORT SORT-WORK-FILE
               ON ASCENDING KEY S-GEN-FLOOR, S-CHIP-FLOOR
               USING PAIR-TABLE GIVING PAIR-TABLE.

           MOVE ELEVATOR OF IN-STATE TO OUT-KEY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM-MATERIALS-VAL
               COMPUTE OUT-KEY = (OUT-KEY * 16) +
                   ((P-GEN-FLOOR(I) - 1) * 4) + (P-CHIP-FLOOR(I) - 1)
           END-PERFORM.

       IS-VISITED USING IN-KEY, GIVING OUT-IS-VISITED-FLAG.
           MOVE 'Y' TO OUT-IS-VISITED-FLAG.
           COMPUTE HASH-IDX = FUNCTION MOD(IN-KEY, HASH-SIZE) + 1.
           MOVE HASH-TABLE(HASH-IDX) TO CURR-NODE-IDX.
           PERFORM UNTIL CURR-NODE-IDX = 0
               IF POOL-KEY(CURR-NODE-IDX) = IN-KEY
                   EXIT PERFORM
               END-IF
               MOVE POOL-NEXT(CURR-NODE-IDX) TO CURR-NODE-IDX
           END-PERFORM.

           IF CURR-NODE-IDX = 0
               MOVE 'N' TO OUT-IS-VISITED-FLAG
               ADD 1 TO NODE-IDX
               MOVE IN-KEY TO POOL-KEY(NODE-IDX)
               MOVE HASH-TABLE(HASH-IDX) TO POOL-NEXT(NODE-IDX)
               MOVE NODE-IDX TO HASH-TABLE(HASH-IDX)
           END-IF.
