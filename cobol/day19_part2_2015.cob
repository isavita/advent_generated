
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Solve.
       AUTHOR. Programmer.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD         PIC X(1024).

       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           05  WS-EOF-FLAG      PIC X VALUE 'N'.
               88  IS-EOF       VALUE 'Y'.

       01  WS-MOLECULE-DATA.
           05  WS-MOLECULE      PIC X(1024).
           05  WS-MOLECULE-LEN  PIC 9(4) COMP.
           05  WS-IDX           PIC 9(4) COMP.

       01  WS-COUNTERS.
           05  WS-TOTAL-ELEMENTS PIC 9(4) COMP VALUE 0.
           05  WS-RN-COUNT       PIC 9(4) COMP VALUE 0.
           05  WS-AR-COUNT       PIC 9(4) COMP VALUE 0.
           05  WS-Y-COUNT        PIC 9(4) COMP VALUE 0.

       01  WS-RESULT.
           05  WS-STEPS          PIC S9(4) COMP.
           05  WS-DISPLAY-STEPS  PIC Z(3)9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-READ-MOLECULE.
           PERFORM 2000-PROCESS-MOLECULE.
           PERFORM 3000-CALCULATE-AND-DISPLAY.
           STOP RUN.

       1000-READ-MOLECULE.
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL IS-EOF
               READ INPUT-FILE
                   AT END SET IS-EOF TO TRUE
               END-READ
               IF INPUT-RECORD = SPACES
                   READ INPUT-FILE INTO WS-MOLECULE
                       AT END SET IS-EOF TO TRUE
                   END-READ
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           CLOSE INPUT-FILE.

       2000-PROCESS-MOLECULE.
           INSPECT FUNCTION REVERSE(WS-MOLECULE)
               TALLYING WS-IDX FOR LEADING SPACES.
           COMPUTE WS-MOLECULE-LEN = LENGTH OF WS-MOLECULE - WS-IDX.

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-MOLECULE-LEN

               ADD 1 TO WS-TOTAL-ELEMENTS

               IF WS-IDX < WS-MOLECULE-LEN AND
                  WS-MOLECULE(WS-IDX + 1 : 1) IS ALPHABETIC-LOWER
                   EVALUATE WS-MOLECULE(WS-IDX : 2)
                       WHEN "Rn"
                           ADD 1 TO WS-RN-COUNT
                       WHEN "Ar"
                           ADD 1 TO WS-AR-COUNT
                   END-EVALUATE
                   ADD 1 TO WS-IDX
               ELSE
                   IF WS-MOLECULE(WS-IDX : 1) = "Y"
                       ADD 1 TO WS-Y-COUNT
                   END-IF
               END-IF
           END-PERFORM.

       3000-CALCULATE-AND-DISPLAY.
           COMPUTE WS-STEPS = WS-TOTAL-ELEMENTS - WS-RN-COUNT
               - WS-AR-COUNT - (2 * WS-Y-COUNT) - 1.
           MOVE WS-STEPS TO WS-DISPLAY-STEPS.
           DISPLAY FUNCTION TRIM(WS-DISPLAY-STEPS).
