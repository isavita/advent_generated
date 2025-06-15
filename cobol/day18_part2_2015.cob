
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AnimateLights.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD           PIC X(100).
       
       WORKING-STORAGE SECTION.
       78  GRID-SIZE              VALUE 100.
       78  STEPS-TO-RUN           VALUE 100.
       
       01  WS-GRID-TABLE.
           05 WS-GRID-ROW OCCURS GRID-SIZE TIMES.
               10 WS-GRID-CELL PIC 9 OCCURS GRID-SIZE TIMES.
       
       01  WS-NEW-GRID-TABLE.
           05 WS-NEW-GRID-ROW OCCURS GRID-SIZE TIMES.
               10 WS-NEW-GRID-CELL PIC 9 OCCURS GRID-SIZE TIMES.
       
       01  WS-VARS.
           05 WS-Y                PIC 9(3).
           05 WS-X                PIC 9(3).
           05 WS-DY               PIC S9(1).
           05 WS-DX               PIC S9(1).
           05 WS-NY               PIC S9(3).
           05 WS-NX               PIC S9(3).
           05 WS-ON-NEIGHBORS     PIC 9(1).
           05 WS-ON-COUNT         PIC 9(5) VALUE 0.
           05 WS-DISPLAY-COUNT    PIC ZZZZ9.
       
       PROCEDURE DIVISION.
       MAIN.
           PERFORM 1000-READ-INPUT.
           PERFORM 2000-SET-INITIAL-CORNERS.
       
           PERFORM STEPS-TO-RUN TIMES
               PERFORM 3000-RUN-STEP
           END-PERFORM.
       
           PERFORM 4000-COUNT-FINAL-LIGHTS.
       
           MOVE WS-ON-COUNT TO WS-DISPLAY-COUNT.
           DISPLAY FUNCTION TRIM(WS-DISPLAY-COUNT).
           STOP RUN.
       
       1000-READ-INPUT.
           OPEN INPUT INPUT-FILE.
           PERFORM VARYING WS-Y FROM 1 BY 1 UNTIL WS-Y > GRID-SIZE
               READ INPUT-FILE
               PERFORM VARYING WS-X FROM 1 BY 1 UNTIL WS-X > GRID-SIZE
                   IF INPUT-RECORD(WS-X:1) = '#'
                       MOVE 1 TO WS-GRID-CELL(WS-Y, WS-X)
                   ELSE
                       MOVE 0 TO WS-GRID-CELL(WS-Y, WS-X)
                   END-IF
               END-PERFORM
           END-PERFORM.
           CLOSE INPUT-FILE.
       
       2000-SET-INITIAL-CORNERS.
           MOVE 1 TO WS-GRID-CELL(1, 1).
           MOVE 1 TO WS-GRID-CELL(1, GRID-SIZE).
           MOVE 1 TO WS-GRID-CELL(GRID-SIZE, 1).
           MOVE 1 TO WS-GRID-CELL(GRID-SIZE, GRID-SIZE).
       
       3000-RUN-STEP.
           PERFORM VARYING WS-Y FROM 1 BY 1 UNTIL WS-Y > GRID-SIZE
               PERFORM VARYING WS-X FROM 1 BY 1 UNTIL WS-X > GRID-SIZE
                   PERFORM 3100-CALCULATE-CELL-STATE
               END-PERFORM
           END-PERFORM.
           PERFORM 3200-SET-STEP-CORNERS.
           MOVE WS-NEW-GRID-TABLE TO WS-GRID-TABLE.
       
       3100-CALCULATE-CELL-STATE.
           PERFORM 3110-COUNT-NEIGHBORS.
           EVALUATE WS-GRID-CELL(WS-Y, WS-X)
               WHEN 1
                   IF WS-ON-NEIGHBORS = 2 OR WS-ON-NEIGHBORS = 3
                       MOVE 1 TO WS-NEW-GRID-CELL(WS-Y, WS-X)
                   ELSE
                       MOVE 0 TO WS-NEW-GRID-CELL(WS-Y, WS-X)
                   END-IF
               WHEN 0
                   IF WS-ON-NEIGHBORS = 3
                       MOVE 1 TO WS-NEW-GRID-CELL(WS-Y, WS-X)
                   ELSE
                       MOVE 0 TO WS-NEW-GRID-CELL(WS-Y, WS-X)
                   END-IF
           END-EVALUATE.
       
       3110-COUNT-NEIGHBORS.
           MOVE 0 TO WS-ON-NEIGHBORS.
           PERFORM VARYING WS-DY FROM -1 BY 1 UNTIL WS-DY > 1
               PERFORM VARYING WS-DX FROM -1 BY 1 UNTIL WS-DX > 1
                   IF NOT (WS-DX = 0 AND WS-DY = 0)
                       COMPUTE WS-NY = WS-Y + WS-DY
                       COMPUTE WS-NX = WS-X + WS-DX
                       IF WS-NY >= 1 AND WS-NY <= GRID-SIZE AND
                          WS-NX >= 1 AND WS-NX <= GRID-SIZE
                           IF WS-GRID-CELL(WS-NY, WS-NX) = 1
                               ADD 1 TO WS-ON-NEIGHBORS
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.
       
       3200-SET-STEP-CORNERS.
           MOVE 1 TO WS-NEW-GRID-CELL(1, 1).
           MOVE 1 TO WS-NEW-GRID-CELL(1, GRID-SIZE).
           MOVE 1 TO WS-NEW-GRID-CELL(GRID-SIZE, 1).
           MOVE 1 TO WS-NEW-GRID-CELL(GRID-SIZE, GRID-SIZE).
       
       4000-COUNT-FINAL-LIGHTS.
           PERFORM VARYING WS-Y FROM 1 BY 1 UNTIL WS-Y > GRID-SIZE
               PERFORM VARYING WS-X FROM 1 BY 1 UNTIL WS-X > GRID-SIZE
                   IF WS-GRID-CELL(WS-Y, WS-X) = 1
                       ADD 1 TO WS-ON-COUNT
                   END-IF
               END-PERFORM
           END-PERFORM.
