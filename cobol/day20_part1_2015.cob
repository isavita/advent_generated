
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOLVE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD           PIC X(10).
       WORKING-STORAGE SECTION.
       01  WS-INPUT-VAL           PIC 9(10).
       01  WS-RESULT              PIC Z(7).
       01  WS-I                   PIC 9(7) COMP.
       01  WS-J                   PIC 9(7) COMP.
       01  WS-TEMP-CALC           PIC 9(8) COMP.

       01  WS-HOUSES-TABLE.
           02 WS-HOUSES OCCURS 1000000 TIMES
              PIC 9(8) COMP VALUE 0.

       PROCEDURE DIVISION.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE.
           CLOSE INPUT-FILE.
           MOVE FUNCTION NUMVAL(INPUT-RECORD) TO WS-INPUT-VAL.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 999999
               COMPUTE WS-TEMP-CALC = WS-I * 10
               PERFORM VARYING WS-J FROM WS-I BY WS-I
                   UNTIL WS-J > 999999
                   ADD WS-TEMP-CALC TO WS-HOUSES(WS-J)
               END-PERFORM
           END-PERFORM.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 999999
               IF WS-HOUSES(WS-I) >= WS-INPUT-VAL
                   MOVE WS-I TO WS-RESULT
                   DISPLAY FUNCTION TRIM(WS-RESULT)
                   STOP RUN
               END-IF
           END-PERFORM.

           STOP RUN.
