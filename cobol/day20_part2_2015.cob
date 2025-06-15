
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
       01  INPUT-RECORD         PIC X(20).
       WORKING-STORAGE SECTION.
       01  WS-INPUT-STR         PIC X(20).
       01  WS-INPUT-NUM         PIC 9(10).
       01  WS-TARGET            PIC 9(10).
       01  WS-ELF               PIC 9(10) COMP-5.
       01  WS-HOUSE             PIC 9(10) COMP-5.
       01  WS-I                 PIC 9(10) COMP-5.
       01  WS-LIMIT             PIC 9(10) COMP-5.

       01  HOUSES-TABLE.
           05 HOUSES OCCURS 3100000 TIMES PIC 9(10) COMP-5.

       PROCEDURE DIVISION.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO WS-INPUT-STR.
           CLOSE INPUT-FILE.

           COMPUTE WS-INPUT-NUM = FUNCTION NUMVAL(WS-INPUT-STR).
           DIVIDE 11 INTO WS-INPUT-NUM GIVING WS-TARGET.

           INITIALIZE HOUSES-TABLE.

           PERFORM VARYING WS-ELF FROM 1 BY 1
               UNTIL WS-ELF > WS-TARGET
               COMPUTE WS-LIMIT = WS-ELF * 50
               PERFORM VARYING WS-HOUSE FROM WS-ELF BY WS-ELF
                   UNTIL WS-HOUSE > WS-TARGET OR WS-HOUSE > WS-LIMIT
                   ADD WS-ELF TO HOUSES(WS-HOUSE)
               END-PERFORM
           END-PERFORM.

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-TARGET
               IF HOUSES(WS-I) >= WS-TARGET
                   DISPLAY WS-I
                   STOP RUN
               END-IF
           END-PERFORM.

           STOP RUN.
