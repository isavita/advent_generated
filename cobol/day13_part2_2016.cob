
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Solve.
       AUTHOR. Expert Programmer.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Constants.
           05 FAV-NUM              PIC 9(4) VALUE 1362.
           05 MAX-STEPS            PIC 9(2) VALUE 50.
           05 MAX-COORD            PIC 9(3) VALUE 100.
           05 QUEUE-MAX-SIZE       PIC 9(5) VALUE 10000.

       01  BFS-State.
           05 WS-FRONT             PIC 9(5) COMP.
           05 WS-REAR              PIC 9(5) COMP.
           05 WS-QUEUE-SIZE        PIC 9(5) COMP.
           05 WS-STEPS             PIC 9(2) COMP.
           05 WS-REACHABLE-COUNT   PIC 9(5) COMP VALUE 0.

       01  Point-Structure.
           05 P-CURRENT.
              10 CURR-X            PIC S9(4) COMP.
              10 CURR-Y            PIC S9(4) COMP.
           05 P-NEXT.
              10 NEXT-X            PIC S9(4) COMP.
              10 NEXT-Y            PIC S9(4) COMP.

       01  Wall-Check-Vars.
           05 WC-NUM               PIC S9(18) COMP.
           05 WC-BITS              PIC 9(4) COMP.
           05 WC-REMAINDER         PIC 9(1) COMP.
           05 WC-IS-WALL-FLAG      PIC 9(1).

       01  Loop-Counters.
           05 I                    PIC 9(3) COMP.
           05 J                    PIC 9(3) COMP.
           05 K                    PIC 9(5) COMP.

       01  Delta-Table.
           05 DELTA-VALUES.
              10 FILLER            PIC S9(1) VALUE +1.
              10 FILLER            PIC S9(1) VALUE +0.
              10 FILLER            PIC S9(1) VALUE -1.
              10 FILLER            PIC S9(1) VALUE +0.
              10 FILLER            PIC S9(1) VALUE +0.
              10 FILLER            PIC S9(1) VALUE +1.
              10 FILLER            PIC S9(1) VALUE +0.
              10 FILLER            PIC S9(1) VALUE -1.
           05 DELTA-REDEFINED REDEFINES DELTA-VALUES.
              10 DELTA-ENTRY OCCURS 4 TIMES INDEXED BY D-IDX.
                 15 DELTA-X        PIC S9(1).
                 15 DELTA-Y        PIC S9(1).

       01  Visited-Table.
           05 VISITED-ROW OCCURS 100 TIMES.
              10 VISITED-COL OCCURS 100 TIMES PIC 9(1).

       01  Queue-Table.
           05 QUEUE-ENTRY OCCURS 10000 TIMES.
              10 Q-X               PIC S9(4) COMP.
              10 Q-Y               PIC S9(4) COMP.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM INITIALIZE-BFS.

           PERFORM VARYING WS-STEPS FROM 1 BY 1
               UNTIL WS-STEPS > MAX-STEPS
               COMPUTE WS-QUEUE-SIZE = WS-REAR - WS-FRONT
               IF WS-QUEUE-SIZE > 0
                   PERFORM PROCESS-QUEUE-LEVEL WS-QUEUE-SIZE TIMES
               END-IF
           END-PERFORM.

           PERFORM COUNT-VISITED.
           DISPLAY WS-REACHABLE-COUNT.
           STOP RUN.

       INITIALIZE-BFS.
           INITIALIZE Visited-Table.
           MOVE 1 TO WS-FRONT WS-REAR.
           MOVE 1 TO Q-X(WS-REAR) Q-Y(WS-REAR).
           MOVE 1 TO VISITED-COL(1 + 1, 1 + 1).
           ADD 1 TO WS-REAR.

       PROCESS-QUEUE-LEVEL.
           MOVE Q-X(WS-FRONT) TO CURR-X.
           MOVE Q-Y(WS-FRONT) TO CURR-Y.
           ADD 1 TO WS-FRONT.
           PERFORM VARYING D-IDX FROM 1 BY 1 UNTIL D-IDX > 4
               PERFORM EXPLORE-NEIGHBOR
           END-PERFORM.

       EXPLORE-NEIGHBOR.
           COMPUTE NEXT-X = CURR-X + DELTA-X(D-IDX).
           COMPUTE NEXT-Y = CURR-Y + DELTA-Y(D-IDX).

           IF NEXT-X < 0 OR NEXT-Y < 0
               OR NEXT-X >= MAX-COORD OR NEXT-Y >= MAX-COORD
               EXIT PARAGRAPH
           END-IF.

           IF VISITED-COL(NEXT-X + 1, NEXT-Y + 1) = 1
               EXIT PARAGRAPH
           END-IF.

           PERFORM CHECK-IF-WALL.
           IF WC-IS-WALL-FLAG = 0
               MOVE 1 TO VISITED-COL(NEXT-X + 1, NEXT-Y + 1)
               MOVE NEXT-X TO Q-X(WS-REAR)
               MOVE NEXT-Y TO Q-Y(WS-REAR)
               ADD 1 TO WS-REAR
           END-IF.

       CHECK-IF-WALL.
           COMPUTE WC-NUM = (NEXT-X * NEXT-X) + (3 * NEXT-X)
               + (2 * NEXT-X * NEXT-Y) + NEXT-Y + (NEXT-Y * NEXT-Y)
               + FAV-NUM.
           MOVE 0 TO WC-BITS.
           PERFORM UNTIL WC-NUM = 0
               DIVIDE WC-NUM BY 2 GIVING WC-NUM REMAINDER WC-REMAINDER
               IF WC-REMAINDER = 1
                   ADD 1 TO WC-BITS
               END-IF
           END-PERFORM.
           DIVIDE WC-BITS BY 2 GIVING I REMAINDER WC-REMAINDER.
           MOVE WC-REMAINDER TO WC-IS-WALL-FLAG.

       COUNT-VISITED.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MAX-COORD
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > MAX-COORD
                   IF VISITED-COL(I, J) = 1
                       ADD 1 TO WS-REACHABLE-COUNT
                   END-IF
               END-PERFORM
           END-PERFORM.
