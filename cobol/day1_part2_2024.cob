
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOLPYTHON.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD               PIC X(256).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS             PIC X(02) VALUE SPACES.
           88 WS-END-OF-FILE                    VALUE "10".

       01  WS-INPUT-LINE-DATA.
           05 WS-LOCATION-ID-STR      PIC X(10).
           05 WS-SIMILARITY-STR       PIC X(10).

       01  WS-TABLE-CONTROL.
           05 WS-MAX-ENTRIES          PIC 9(04) VALUE 1000.
           05 WS-ENTRY-COUNT          PIC 9(04) VALUE 0.
           05 WS-IDX1                 PIC 9(04).
           05 WS-IDX2                 PIC 9(04).

       01  WS-LOCATION-DATA-TABLE.
           05 WS-LOCATION-ENTRY OCCURS 1000 TIMES.
              10 WS-STORED-LOCATION-ID   PIC 9(09).
              10 WS-STORED-SIMILARITY    PIC 9(09).

       01  WS-CALCULATION-VARS.
           05 WS-TOTAL                PIC 9(18) VALUE 0.
           05 WS-COUNT                PIC 9(09) VALUE 0.
           05 WS-TEMP-LOCATION-ID     PIC 9(09).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE.
           PERFORM READ-INPUT-FILE
               UNTIL WS-END-OF-FILE OR WS-ENTRY-COUNT >= WS-MAX-ENTRIES.
           CLOSE INPUT-FILE.

           PERFORM CALCULATE-TOTAL.

           DISPLAY WS-TOTAL.

           STOP RUN.

       READ-INPUT-FILE.
           READ INPUT-FILE INTO INPUT-RECORD
               AT END SET WS-END-OF-FILE TO TRUE
               NOT AT END
                   ADD 1 TO WS-ENTRY-COUNT
                   IF WS-ENTRY-COUNT <= WS-MAX-ENTRIES THEN
                       UNSTRING INPUT-RECORD DELIMITED BY ALL SPACES
                           INTO WS-LOCATION-ID-STR
                                WS-SIMILARITY-STR
                       END-UNSTRING
                       MOVE FUNCTION NUMVAL(WS-LOCATION-ID-STR)
                           TO WS-STORED-LOCATION-ID(WS-ENTRY-COUNT)
                       MOVE FUNCTION NUMVAL(WS-SIMILARITY-STR)
                           TO WS-STORED-SIMILARITY(WS-ENTRY-COUNT)
                   ELSE
                       SET WS-END-OF-FILE TO TRUE
                   END-IF
           END-READ.

       CALCULATE-TOTAL.
           PERFORM VARYING WS-IDX1 FROM 1 BY 1
               UNTIL WS-IDX1 > WS-ENTRY-COUNT

               MOVE WS-STORED-LOCATION-ID(WS-IDX1) TO WS-TEMP-LOCATION-ID
               MOVE 0 TO WS-COUNT

               PERFORM VARYING WS-IDX2 FROM 1 BY 1
                   UNTIL WS-IDX2 > WS-ENTRY-COUNT

                   IF WS-STORED-SIMILARITY(WS-IDX2) = WS-TEMP-LOCATION-ID THEN
                       ADD 1 TO WS-COUNT
                   END-IF
               END-PERFORM

               COMPUTE WS-TOTAL = WS-TOTAL + (WS-COUNT * WS-TEMP-LOCATION-ID)

           END-PERFORM.
