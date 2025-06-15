
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GameOfLife.

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
       01  CONSTANTS.
           05 GRID-SIZE         PIC S9(4) COMP VALUE 100.
           05 STEPS             PIC S9(4) COMP VALUE 100.

       01  GRID-TABLES.
           05 GRID-TABLE.
               10 GRID-ROW OCCURS 100 TIMES.
                   15 GRID-CELL PIC 9(1) OCCURS 100 TIMES.
           05 TEMP-TABLE.
               10 TEMP-ROW OCCURS 100 TIMES.
                   15 TEMP-CELL PIC 9(1) OCCURS 100 TIMES.

       01  WORK-AREAS.
           05 I                 PIC S9(4) COMP.
           05 J                 PIC S9(4) COMP.
           05 DX                PIC S9(4) COMP.
           05 DY                PIC S9(4) COMP.
           05 NX                PIC S9(4) COMP.
           05 NY                PIC S9(4) COMP.
           05 NEIGHBOR-COUNT    PIC 9(2)  COMP.
           05 RESULT            PIC 9(5)  COMP VALUE 0.
           05 DISPLAY-RESULT    PIC ZZZZ9.
           05 WS-EOF            PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM READ-INPUT-GRID.
           PERFORM SIMULATE-STEPS STEPS TIMES.
           PERFORM CALCULATE-TOTAL.
           MOVE RESULT TO DISPLAY-RESULT.
           DISPLAY DISPLAY-RESULT.
           STOP RUN.

       READ-INPUT-GRID.
           OPEN INPUT INPUT-FILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRID-SIZE
               READ INPUT-FILE
                   AT END MOVE 'Y' TO WS-EOF
               END-READ
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRID-SIZE
                   IF INPUT-RECORD(J:1) = '#'
                       MOVE 1 TO GRID-CELL(I, J)
                   ELSE
                       MOVE 0 TO GRID-CELL(I, J)
                   END-IF
               END-PERFORM
           END-PERFORM.
           CLOSE INPUT-FILE.

       SIMULATE-STEPS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRID-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRID-SIZE
                   PERFORM CALCULATE-NEIGHBORS
                   PERFORM APPLY-RULES
               END-PERFORM
           END-PERFORM.
           MOVE TEMP-TABLE TO GRID-TABLE.

       CALCULATE-NEIGHBORS.
           MOVE 0 TO NEIGHBOR-COUNT.
           PERFORM VARYING DX FROM -1 BY 1 UNTIL DX > 1
               PERFORM VARYING DY FROM -1 BY 1 UNTIL DY > 1
                   IF DX = 0 AND DY = 0
                       CONTINUE
                   ELSE
                       COMPUTE NX = I + DX
                       COMPUTE NY = J + DY
                       IF NX > 0 AND NX <= GRID-SIZE AND
                          NY > 0 AND NY <= GRID-SIZE
                           ADD GRID-CELL(NX, NY) TO NEIGHBOR-COUNT
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.

       APPLY-RULES.
           EVALUATE TRUE
               WHEN GRID-CELL(I, J) = 1 AND
                    (NEIGHBOR-COUNT = 2 OR NEIGHBOR-COUNT = 3)
                   MOVE 1 TO TEMP-CELL(I, J)
               WHEN GRID-CELL(I, J) = 0 AND NEIGHBOR-COUNT = 3
                   MOVE 1 TO TEMP-CELL(I, J)
               WHEN OTHER
                   MOVE 0 TO TEMP-CELL(I, J)
           END-EVALUATE.

       CALCULATE-TOTAL.
           MOVE 0 TO RESULT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > GRID-SIZE
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > GRID-SIZE
                   ADD GRID-CELL(I, J) TO RESULT
               END-PERFORM
           END-PERFORM.
