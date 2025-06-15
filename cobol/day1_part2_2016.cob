
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FirstRevisited.
       AUTHOR. Programmer.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD         PIC X(4096).

       WORKING-STORAGE SECTION.
       01  WS-INPUT-LINE        PIC X(4096).
       01  WS-INPUT-PTR         PIC 9(4) COMP VALUE 1.
       01  WS-INPUT-LEN         PIC 9(4) COMP.

       01  WS-CURRENT-MOVE      PIC X(10).
       01  WS-TURN              PIC X(1).
       01  WS-BLOCKS            PIC 9(4) COMP.

       01  WS-POS-X             PIC S9(4) COMP VALUE 0.
       01  WS-POS-Y             PIC S9(4) COMP VALUE 0.
       01  WS-DIR-INDEX         PIC 9(1) COMP VALUE 1.

       01  WS-FOUND-FLAG        PIC X(1) VALUE 'N'.
           88 WS-LOCATION-FOUND VALUE 'Y'.

       01  WS-ANSWER            PIC S9(8).
       01  WS-ANSWER-DISPLAY    PIC ZZZZZZZ9.

       01  WS-LOOP-I            PIC 9(4) COMP.
       01  WS-LOOP-J            PIC 9(4) COMP.

       01  DIRECTIONS-TABLE.
           05 FILLER PIC S9(2) COMP VALUE +0. 05 FILLER PIC S9(2) COMP VALUE +1.
           05 FILLER PIC S9(2) COMP VALUE +1. 05 FILLER PIC S9(2) COMP VALUE +0.
           05 FILLER PIC S9(2) COMP VALUE +0. 05 FILLER PIC S9(2) COMP VALUE -1.
           05 FILLER PIC S9(2) COMP VALUE -1. 05 FILLER PIC S9(2) COMP VALUE +0.
       01  DIRECTIONS-ARRAY REDEFINES DIRECTIONS-TABLE.
           05 DIRECTION-VEC OCCURS 4 TIMES.
               10 VEC-DX         PIC S9(2) COMP.
               10 VEC-DY         PIC S9(2) COMP.

       01  WS-VISITED-GRID.
           05 WS-ROW OCCURS 1001 TIMES.
               10 WS-COL OCCURS 1001 TIMES.
                   15 WS-VISITED-FLAG PIC 9(1) VALUE 0.

       01  WS-VISITED-X         PIC 9(4) COMP.
       01  WS-VISITED-Y         PIC 9(4) COMP.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO WS-INPUT-LINE.
           CLOSE INPUT-FILE.

           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-INPUT-LINE))
             TO WS-INPUT-LEN.
           MOVE 1 TO WS-VISITED-FLAG(501, 501).

           PERFORM UNTIL WS-LOCATION-FOUND OR WS-INPUT-PTR > WS-INPUT-LEN
               PERFORM VARYING WS-LOOP-I FROM WS-INPUT-PTR BY 1
                   UNTIL WS-INPUT-LINE(WS-LOOP-I:1) = ',' OR
                         WS-LOOP-I > WS-INPUT-LEN
               END-PERFORM

               COMPUTE WS-BLOCKS = WS-LOOP-I - WS-INPUT-PTR
               MOVE WS-INPUT-LINE(WS-INPUT-PTR:WS-BLOCKS)
                 TO WS-CURRENT-MOVE

               COMPUTE WS-INPUT-PTR = WS-LOOP-I + 2

               MOVE WS-CURRENT-MOVE(1:1) TO WS-TURN
               COMPUTE WS-BLOCKS =
                   FUNCTION NUMVAL(WS-CURRENT-MOVE(2:))

               IF WS-TURN = 'R'
                   ADD 1 TO WS-DIR-INDEX
                   IF WS-DIR-INDEX > 4
                       MOVE 1 TO WS-DIR-INDEX
                   END-IF
               ELSE
                   SUBTRACT 1 FROM WS-DIR-INDEX
                   IF WS-DIR-INDEX = 0
                       MOVE 4 TO WS-DIR-INDEX
                   END-IF
               END-IF

               PERFORM VARYING WS-LOOP-J FROM 1 BY 1
                   UNTIL WS-LOOP-J > WS-BLOCKS OR WS-LOCATION-FOUND

                   ADD VEC-DX(WS-DIR-INDEX) TO WS-POS-X
                   ADD VEC-DY(WS-DIR-INDEX) TO WS-POS-Y

                   COMPUTE WS-VISITED-X = WS-POS-X + 501
                   COMPUTE WS-VISITED-Y = WS-POS-Y + 501

                   IF WS-VISITED-FLAG(WS-VISITED-X, WS-VISITED-Y) = 1
                       SET WS-LOCATION-FOUND TO TRUE
                   ELSE
                       MOVE 1 TO
                           WS-VISITED-FLAG(WS-VISITED-X, WS-VISITED-Y)
                   END-IF
               END-PERFORM
           END-PERFORM.

           COMPUTE WS-ANSWER = FUNCTION ABS(WS-POS-X)
                             + FUNCTION ABS(WS-POS-Y).
           MOVE WS-ANSWER TO WS-ANSWER-DISPLAY.
           DISPLAY FUNCTION TRIM(WS-ANSWER-DISPLAY).

           STOP RUN.
