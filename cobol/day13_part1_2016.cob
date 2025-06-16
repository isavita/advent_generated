
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAZE-SOLVER.
       AUTHOR. Your Name.
       DATE-WRITTEN. 2023-11-27.
       REMARKS.
           This program solves Advent of Code Day 13, Part 1.
           It reads a favorite number from 'input.txt', then
           finds the shortest path from (1,1) to (31,39) in a
           dynamically generated maze using Breadth-First Search (BFS).
           The maze cell type (wall/open) is determined by a formula
           involving the coordinates and the favorite number.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'input.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       * --- Input and Control Variables ---
       01  WS-FAVORITE-NUMBER PIC 9(9) COMP.
       01  WS-EOF-FLAG        PIC X(1) VALUE 'N'.
           88 EOF             VALUE 'Y'.
       01  WS-FOUND-TARGET    PIC X(1) VALUE 'N'.
           88 TARGET-REACHED  VALUE 'Y'.

       * --- BFS Queue Structure ---
       * Max queue size: A 150x150 grid has 22500 cells.
       * Each cell can be enqueued once. 25000 is a safe upper bound.
       01  WS-QUEUE-TABLE.
           05 WS-QUEUE-ENTRY OCCURS 25000 TIMES.
              10 QE-X     PIC 9(3) COMP.
              10 QE-Y     PIC 9(3) COMP.
              10 QE-STEPS PIC 9(3) COMP.

       01  WS-QUEUE-POINTERS.
           05 WS-QUEUE-FRONT PIC 9(5) COMP VALUE 1.
           05 WS-QUEUE-REAR  PIC 9(5) COMP VALUE 0.
           05 WS-QUEUE-SIZE  PIC 9(5) COMP VALUE 0.

       * --- Visited Grid ---
       * Coordinates are 0-indexed, COBOL arrays are 1-indexed.
       * Max X/Y for target (31,39) plus potential path length (e.g., 100 steps)
       * suggests coordinates up to ~130. Using 150 for safety.
       01  WS-VISITED-GRID.
           05 WS-VISITED-ROW OCCURS 150 TIMES.
              10 WS-VISITED-COL OCCURS 150 TIMES PIC X(1) VALUE 'N'.
                 88 IS-VISITED VALUE 'Y'.
                 88 NOT-VISITED VALUE 'N'.

       * --- Current and Neighbor Position Variables ---
       01  WS-CURRENT-POS.
           05 WS-CURRENT-X     PIC 9(3) COMP.
           05 WS-CURRENT-Y     PIC 9(3) COMP.
           05 WS-CURRENT-STEPS PIC 9(3) COMP.

       01  WS-NEIGHBOR-POS.
           05 WS-NEXT-X        PIC 9(3) COMP.
           05 WS-NEXT-Y        PIC 9(3) COMP.

       * --- Directions for BFS (Up, Down, Left, Right) ---
       01  WS-DIRECTIONS.
           05 WS-DX OCCURS 4 TIMES PIC S9(1) COMP VALUE 0, 0, 1, -1.
           05 WS-DY OCCURS 4 TIMES PIC S9(1) COMP VALUE 1, -1, 0, 0.
       01  WS-DIR-IDX PIC 9(1) COMP.

       * --- Variables for IsWall Calculation ---
       01  WS-CALC-VARS.
           05 WS-CALC-X          PIC 9(5) COMP.
           05 WS-CALC-Y          PIC 9(5) COMP.
           05 WS-CALC-FAV-NUM    PIC 9(9) COMP.
           05 WS-SUM             PIC 9(18) COMP.
           05 WS-TEMP-SUM        PIC 9(18) COMP.
           05 WS-BIT-COUNT       PIC 9(3) COMP.
           05 WS-REMAINDER       PIC 9(1) COMP.
           05 WS-IS-WALL-FLAG    PIC X(1).
              88 IS-WALL         VALUE 'Y'.
              88 IS-OPEN-SPACE   VALUE 'N'.

       * --- Constants ---
       01  WS-START-X         PIC 9(3) VALUE 1.
       01  WS-START-Y         PIC 9(3) VALUE 1.
       01  WS-TARGET-X        PIC 9(3) VALUE 31.
       01  WS-TARGET-Y        PIC 9(3) VALUE 39.
       01  WS-MAX-DIM         PIC 9(3) VALUE 150. *> Max X/Y coordinate + 1 for array indexing

       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.
           PERFORM 1000-READ-INPUT-FILE.
           PERFORM 2000-INITIALIZE-BFS.
           PERFORM 3000-BFS-LOOP UNTIL WS-QUEUE-SIZE = 0 OR TARGET-REACHED.
           PERFORM 4000-PRINT-RESULT.
           PERFORM 9000-END-PROGRAM.

       1000-READ-INPUT-FILE.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO INPUT-RECORD
               AT END SET EOF TO TRUE
           END-READ.
           IF NOT EOF
               MOVE INPUT-RECORD TO WS-FAVORITE-NUMBER
           ELSE
               DISPLAY "Error: input.txt is empty or missing favorite number."
               PERFORM 9000-END-PROGRAM
           END-IF.
           CLOSE INPUT-FILE.

       2000-INITIALIZE-BFS.
           * Enqueue starting position (1,1) with 0 steps
           ADD 1 TO WS-QUEUE-REAR.
           ADD 1 TO WS-QUEUE-SIZE.
           MOVE WS-START-X TO QE-X(WS-QUEUE-REAR).
           MOVE WS-START-Y TO QE-Y(WS-QUEUE-REAR).
           MOVE 0 TO QE-STEPS(WS-QUEUE-REAR).

           * Mark starting position as visited
           * Add 1 to coordinates for 1-based array indexing
           SET IS-VISITED OF WS-VISITED-COL(WS-START-Y + 1, WS-START-X + 1) TO TRUE.

       3000-BFS-LOOP.
           * Dequeue current position
           MOVE QE-X(WS-QUEUE-FRONT) TO WS-CURRENT-X.
           MOVE QE-Y(WS-QUEUE-FRONT) TO WS-CURRENT-Y.
           MOVE QE-STEPS(WS-QUEUE-FRONT) TO WS-CURRENT-STEPS.
           ADD 1 TO WS-QUEUE-FRONT.
           SUBTRACT 1 FROM WS-QUEUE-SIZE.

           * Check if target reached
           IF WS-CURRENT-X = WS-TARGET-X AND WS-CURRENT-Y = WS-TARGET-Y
               SET TARGET-REACHED TO TRUE
               EXIT SECTION
           END-IF.

           * Explore neighbors (Up, Down, Left, Right)
           PERFORM VARYING WS-DIR-IDX FROM 1 BY 1 UNTIL WS-DIR-IDX > 4
               COMPUTE WS-NEXT-X = WS-CURRENT-X + WS-DX(WS-DIR-IDX)
               COMPUTE WS-NEXT-Y = WS-CURRENT-Y + WS-DY(WS-DIR-IDX)

               * Check if valid coordinates (non-negative and within grid bounds)
               IF WS-NEXT-X >= 0 AND WS-NEXT-Y >= 0 AND
                  WS-NEXT-X < WS-MAX-DIM AND WS-NEXT-Y < WS-MAX-DIM

                   * Check if not visited
                   * Add 1 to coordinates for 1-based array indexing
                   IF NOT IS-VISITED OF WS-VISITED-COL(WS-NEXT-Y + 1, WS-NEXT-X + 1)

                       * Prepare parameters for IS-WALL-CHECK
                       MOVE WS-NEXT-X TO WS-CALC-X.
                       MOVE WS-NEXT-Y TO WS-CALC-Y.
                       MOVE WS-FAVORITE-NUMBER TO WS-CALC-FAV-NUM.

                       * Perform the wall/open space check
                       PERFORM 3100-IS-WALL-CHECK.

                       IF IS-OPEN-SPACE
                           * Mark as visited
                           SET IS-VISITED OF WS-VISITED-COL(WS-NEXT-Y + 1, WS-NEXT-X + 1) TO TRUE

                           * Enqueue neighbor
                           ADD 1 TO WS-QUEUE-REAR
                           ADD 1 TO WS-QUEUE-SIZE
                           MOVE WS-NEXT-X TO QE-X(WS-QUEUE-REAR)
                           MOVE WS-NEXT-Y TO QE-Y(WS-QUEUE-REAR)
                           ADD 1 TO WS-CURRENT-STEPS GIVING QE-STEPS(WS-QUEUE-REAR)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
           EXIT SECTION.

       3100-IS-WALL-CHECK SECTION.
           * This section determines if a given coordinate (WS-CALC-X, WS-CALC-Y)
           * is a wall or an open space based on WS-CALC-FAV-NUM.
           * The result is stored in WS-IS-WALL-FLAG.

           MOVE 0 TO WS-SUM
                     WS-BIT-COUNT.

           * Calculate x*x + 3*x + 2*x*y + y + y*y + favorite_number
           COMPUTE WS-SUM = (WS-CALC-X * WS-CALC-X) + (3 * WS-CALC-X) +
                            (2 * WS-CALC-X * WS-CALC-Y) + WS-CALC-Y +
                            (WS-CALC-Y * WS-CALC-Y) + WS-CALC-FAV-NUM.

           * Count set bits (1s) in the binary representation of WS-SUM
           MOVE WS-SUM TO WS-TEMP-SUM.
           PERFORM UNTIL WS-TEMP-SUM = 0
               DIVIDE 2 INTO WS-TEMP-SUM GIVING WS-TEMP-SUM
                                         REMAINDER WS-REMAINDER
               IF WS-REMAINDER = 1
                   ADD 1 TO WS-BIT-COUNT
               END-IF
           END-PERFORM.

           * If number of set bits is even, it's an open space; otherwise, it's a wall.
           IF WS-BIT-COUNT IS EVEN
               SET IS-OPEN-SPACE TO TRUE
           ELSE
               SET IS-WALL TO TRUE
           END-IF.
           EXIT SECTION.

       4000-PRINT-RESULT.
           IF TARGET-REACHED
               DISPLAY "Fewest steps to reach " WS-TARGET-X ", " WS-TARGET-Y ": " WS-CURRENT-STEPS
           ELSE
               DISPLAY "Target " WS-TARGET-X ", " WS-TARGET-Y " not reachable."
           END-IF.

       9000-END-PROGRAM.
           STOP RUN.
