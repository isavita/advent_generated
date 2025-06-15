
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
       01  INPUT-RECORD         PIC X(20).

       WORKING-STORAGE SECTION.
       01  WS-EOF               PIC A(1) VALUE 'N'.
       01  WS-L                 PIC 9(5).
       01  WS-W                 PIC 9(5).
       01  WS-H                 PIC 9(5).
       01  WS-MAX-DIM           PIC 9(5).
       01  WS-PERIMETER         PIC 9(10).
       01  WS-VOLUME            PIC 9(10).
       01  WS-TOTAL-RIBBON      PIC 9(10) VALUE 0.
       01  WS-DISPLAY-RIBBON    PIC Z(9)9.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL WS-EOF = 'Y'
               READ INPUT-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM CALCULATE-RIBBON
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.

           MOVE WS-TOTAL-RIBBON TO WS-DISPLAY-RIBBON.
           DISPLAY FUNCTION TRIM(WS-DISPLAY-RIBBON).
           STOP RUN.

       CALCULATE-RIBBON.
           UNSTRING INPUT-RECORD DELIMITED BY 'x'
               INTO WS-L, WS-W, WS-H
           END-UNSTRING.

           MOVE WS-L TO WS-MAX-DIM.
           IF WS-W > WS-MAX-DIM
               MOVE WS-W TO WS-MAX-DIM
           END-IF.
           IF WS-H > WS-MAX-DIM
               MOVE WS-H TO WS-MAX-DIM
           END-IF.

           COMPUTE WS-PERIMETER = 2 * (WS-L + WS-W + WS-H - WS-MAX-DIM).
           COMPUTE WS-VOLUME = WS-L * WS-W * WS-H.
           ADD WS-PERIMETER, WS-VOLUME TO WS-TOTAL-RIBBON.
