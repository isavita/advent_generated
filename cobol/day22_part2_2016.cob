
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRID-MOVER.
       AUTHOR. EXPERT PROGRAMMER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'input.txt'
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05 FILLER PIC X(255).

       WORKING-STORAGE SECTION.
       01  NODE-DATA.
           05 NODE-X       PIC 9(4).
           05 NODE-Y       PIC 9(4).
           05 NODE-SIZE    PIC 9(4).
           05 NODE-USED    PIC 9(4).
           05 NODE-AVAIL   PIC 9(4).
           05 NODE-USE-PCT PIC 9(3).

       01  GRID-DIMENSIONS.
           05 MAX-DIM      PIC 9(2) VALUE 50.
           05 WALL-THRESHOLD PIC 9(4) VALUE 400.

       01  NODE-RECORD.
           05 NODE-USED-FIELD PIC 9(4).
           05 NODE-AVAIL-FIELD PIC 9(4).

       01  GRID-NODES.
           05 GRID-ROW OCCURS 50 TIMES.
              10 GRID-COL OCCURS 50 TIMES.
                 15 NODE-INFO PIC X(10).

       01  PROGRAM-VARIABLES.
           05 FILE-STATUS      PIC X(2).
           05 LINE-BUFFER      PIC X(255).
           05 CURRENT-X        PIC 9(4).
           05 CURRENT-Y        PIC 9(4).
           05 MAX-X            PIC 9(4) VALUE 0.
           05 MAX-Y            PIC 9(4) VALUE 0.
           05 WIDTH            PIC 9(4).
           05 HEIGHT           PIC 9(4).
           05 HOLE-X           PIC S9(4) VALUE -1.
           05 HOLE-Y           PIC S9(4) VALUE -1.
           05 GOAL-X           PIC 9(4).
           05 GOAL-Y           PIC 9(4).
           05 MOVES-SUM        PIC 9(9) VALUE 0.
           05 BFS-DEPTH        PIC S9(4).
           05 QUEUE-HEAD       PIC 9(4) VALUE 0.
           05 QUEUE-TAIL       PIC 9(4) VALUE 0.
           05 QUEUE-MAX        PIC 9(4) VALUE 2500.

       01  QUEUE-DATA.
           05 QUEUE-X OCCURS 2500 TIMES PIC 9(4).
           05 QUEUE-Y OCCURS 2500 TIMES PIC 9(4).

       01  NEIGHBORS.
           05 NEIGHBOR-DX PIC 9(2) VALUE 0.
           05 NEIGHBOR-DY PIC 9(2) VALUE 1.
           05 NEIGHBOR-DX-2 PIC 9(2) VALUE 0.
           05 NEIGHBOR-DY-2 PIC 9(2) VALUE -1.
           05 NEIGHBOR-DX-3 PIC 9(2) VALUE 1.
           05 NEIGHBOR-DY-3 PIC 9(2) VALUE 0.
           05 NEIGHBOR-DX-4 PIC 9(2) VALUE -1.
           05 NEIGHBOR-DY-4 PIC 9(2) VALUE 0.

       01  TEMP-VARS.
           05 TEMP-X           PIC 9(4).
           05 TEMP-Y           PIC 9(4).
           05 TEMP-GOAL-X      PIC 9(4).
           05 TEMP-GOAL-Y      PIC 9(4).
           05 TEMP-HOLE-X      PIC 9(4).
           05 TEMP-HOLE-Y      PIC 9(4).
           05 TEMP-NODE-USED   PIC 9(4).
           05 TEMP-NODE-AVAIL  PIC 9(4).

       01  BFS-DEPTH-GRID.
           05 DEPTH-ROW OCCURS 50 TIMES.
              10 DEPTH-COL OCCURS 50 TIMES PIC S9(4) VALUE -1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-READ-INPUT.
           PERFORM 3000-CALCULATE-MOVES.
           PERFORM 4000-DISPLAY-RESULT.
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT INPUT-FILE.
           IF FILE-STATUS NOT = '00'
               DISPLAY 'Error opening input.txt'
               STOP RUN
           END-IF.

       2000-READ-INPUT.
           READ INPUT-FILE RECORD.
           IF FILE-STATUS NOT = '00'
               DISPLAY 'Error reading header'
               CLOSE INPUT-FILE
               STOP RUN
           END-IF.
           READ INPUT-FILE RECORD.
           IF FILE-STATUS NOT = '00'
               DISPLAY 'Error reading header'
               CLOSE INPUT-FILE
               STOP RUN
           END-IF.

           PERFORM UNTIL FILE-STATUS NOT = '00'
               READ INPUT-FILE RECORD
               IF FILE-STATUS = '10'
                   EXIT PERFORM
               END-IF

               UNSTRING LINE-BUFFER
                   DELIMITED BY '/dev/grid/node-x'
                   INTO FILLER, NODE-X, FILLER
               END-UNSTRING.
               UNSTRING LINE-BUFFER
                   DELIMITED BY '-y'
                   INTO FILLER, NODE-Y, FILLER
               END-UNSTRING.
               UNSTRING LINE-BUFFER
                   DELIMITED BY 'T '
                   INTO FILLER, NODE-SIZE, FILLER
               END-UNSTRING.
               UNSTRING LINE-BUFFER
                   DELIMITED BY 'T '
                   INTO FILLER, NODE-USED, FILLER
               END-UNSTRING.
               UNSTRING LINE-BUFFER
                   DELIMITED BY 'T '
                   INTO FILLER, NODE-AVAIL, FILLER
               END-UNSTRING.
               UNSTRING LINE-BUFFER
                   DELIMITED BY '%'
                   INTO FILLER, NODE-USE-PCT, FILLER
               END-UNSTRING.

               IF NODE-X >= MAX-DIM OR NODE-Y >= MAX-DIM
                   DISPLAY 'Error: Grid dimensions exceed MAX_DIM'
                   CLOSE INPUT-FILE
                   STOP RUN
               END-IF

               MOVE NODE-USED TO NODE-USED-FIELD.
               MOVE NODE-AVAIL TO NODE-AVAIL-FIELD.
               MOVE NODE-USED-FIELD TO GRID-COL(NODE-X + 1, NODE-Y + 1) OF GRID-NODES.
               MOVE NODE-AVAIL-FIELD TO GRID-COL(NODE-X + 1, NODE-Y + 1) OF GRID-NODES + 1.

               IF NODE-X > MAX-X
                   MOVE NODE-X TO MAX-X
               END-IF.
               IF NODE-Y > MAX-Y
                   MOVE NODE-Y TO MAX-Y
               END-IF
           END-PERFORM.

           MOVE MAX-X + 1 TO WIDTH.
           MOVE MAX-Y + 1 TO HEIGHT.
           CLOSE INPUT-FILE.

           PERFORM VARYING CURRENT-Y FROM 0 BY 1 UNTIL CURRENT-Y >= HEIGHT
               PERFORM VARYING CURRENT-X FROM 0 BY 1 UNTIL CURRENT-X >= WIDTH
                   IF GRID-COL(CURRENT-X, CURRENT-Y) OF GRID-NODES = 0
                       MOVE CURRENT-X TO HOLE-X
                       MOVE CURRENT-Y TO HOLE-Y
                       GO TO FOUND-HOLE
                   END-IF
               END-PERFORM
           END-PERFORM.
       FOUND-HOLE.

       3000-CALCULATE-MOVES.
           MOVE WIDTH - 1 TO GOAL-X.
           MOVE 0 TO GOAL-Y.

           IF HOLE-X = -1
               MOVE -1 TO MOVES-SUM
               EXIT PARAGRAPH
           END-IF.

           PERFORM UNTIL GOAL-X = 0 AND GOAL-Y = 0
               MOVE GOAL-X TO TEMP-GOAL-X.
               MOVE GOAL-Y TO TEMP-GOAL-Y.

               MOVE GOAL-X - 1 TO TEMP-HOLE-X.
               MOVE GOAL-Y TO TEMP-HOLE-Y.

               PERFORM VARYING CURRENT-X FROM 0 BY 1 UNTIL CURRENT-X >= WIDTH
                   PERFORM VARYING CURRENT-Y FROM 0 BY 1 UNTIL CURRENT-Y >= HEIGHT
                       MOVE -1 TO DEPTH-COL(CURRENT-X, CURRENT-Y) OF DEPTH-GRID
                   END-PERFORM
               END-PERFORM.

               MOVE 0 TO QUEUE-HEAD.
               MOVE 0 TO QUEUE-TAIL.

               MOVE HOLE-X TO QUEUE-X(QUEUE-TAIL).
               MOVE HOLE-Y TO QUEUE-Y(QUEUE-TAIL).
               ADD 1 TO QUEUE-TAIL.

               MOVE 0 TO DEPTH-COL(HOLE-X, HOLE-Y) OF DEPTH-GRID.

               PERFORM UNTIL QUEUE-HEAD = QUEUE-TAIL
                   MOVE QUEUE-X(QUEUE-HEAD) TO CURRENT-X.
                   MOVE QUEUE-Y(QUEUE-HEAD) TO CURRENT-Y.
                   ADD 1 TO QUEUE-HEAD.

                   IF CURRENT-X = TEMP-HOLE-X AND CURRENT-Y = TEMP-HOLE-Y
                       ADD DEPTH-COL(CURRENT-X, CURRENT-Y) OF DEPTH-GRID TO MOVES-SUM
                       MOVE CURRENT-X TO HOLE-X
                       MOVE CURRENT-Y TO HOLE-Y
                       GO TO MOVES-CALCULATED
                   END-IF.

                   MOVE DEPTH-COL(CURRENT-X, CURRENT-Y) OF DEPTH-GRID TO BFS-DEPTH.
                   ADD 1 TO BFS-DEPTH.

                   MOVE 0 TO NEIGHBOR-DX.
                   MOVE 1 TO NEIGHBOR-DY.
                   PERFORM 3100-PROCESS-NEIGHBOR.

                   MOVE 0 TO NEIGHBOR-DX-2.
                   MOVE -1 TO NEIGHBOR-DY-2.
                   PERFORM 3100-PROCESS-NEIGHBOR.

                   MOVE 1 TO NEIGHBOR-DX-3.
                   MOVE 0 TO NEIGHBOR-DY-3.
                   PERFORM 3100-PROCESS-NEIGHBOR.

                   MOVE -1 TO NEIGHBOR-DX-4.
                   MOVE 0 TO NEIGHBOR-DY-4.
                   PERFORM 3100-PROCESS-NEIGHBOR.
               END-PERFORM.
               MOVE -1 TO MOVES-SUM
               EXIT PARAGRAPH.
           MOVES-CALCULATED.

               ADD 1 TO MOVES-SUM.

               MOVE TEMP-GOAL-X TO TEMP-NODE-USED.
               MOVE TEMP-GOAL-Y TO TEMP-NODE-AVAIL.
               MOVE HOLE-X TO GOAL-X.
               MOVE HOLE-Y TO GOAL-Y.
               MOVE TEMP-NODE-USED TO HOLE-X.
               MOVE TEMP-NODE-AVAIL TO HOLE-Y.
           END-PERFORM.

       3100-PROCESS-NEIGHBOR.
           MOVE CURRENT-X TO TEMP-X.
           MOVE CURRENT-Y TO TEMP-Y.

           ADD NEIGHBOR-DX TO TEMP-X.
           ADD NEIGHBOR-DY TO TEMP-Y.

           IF TEMP-X >= 0 AND TEMP-X < WIDTH AND TEMP-Y >= 0 AND TEMP-Y < HEIGHT
               IF NOT (TEMP-X = GOAL-X AND TEMP-Y = GOAL-Y)
                   IF GRID-COL(TEMP-X, TEMP-Y) OF GRID-NODES < WALL-THRESHOLD
                       IF DEPTH-COL(TEMP-X, TEMP-Y) OF DEPTH-GRID = -1
                           MOVE BFS-DEPTH TO DEPTH-COL(TEMP-X, TEMP-Y) OF DEPTH-GRID
                           MOVE TEMP-X TO QUEUE-X(QUEUE-TAIL).
                           MOVE TEMP-Y TO QUEUE-Y(QUEUE-TAIL).
                           ADD 1 TO QUEUE-TAIL.
                       END-IF
                   END-IF
               END-IF
           END-IF.

       4000-DISPLAY-RESULT.
           IF MOVES-SUM NOT = -1
               DISPLAY MOVES-SUM
           ELSE
               DISPLAY 'Error calculating moves'
           END-IF.
