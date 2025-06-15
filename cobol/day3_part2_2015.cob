
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SantaRobo.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD         PIC X(32767).
       WORKING-STORAGE SECTION.
       01  WS-GRID.
           05 WS-ROW OCCURS 1000 TIMES.
              10 WS-COL PIC 9(1) OCCURS 1000 TIMES VALUE 0.
       01  WS-VARS.
           05 WS-SANTA-X        PIC S9(4) COMP VALUE 501.
           05 WS-SANTA-Y        PIC S9(4) COMP VALUE 501.
           05 WS-ROBO-X         PIC S9(4) COMP VALUE 501.
           05 WS-ROBO-Y         PIC S9(4) COMP VALUE 501.
           05 WS-SANTA-TURN     PIC 9(1)  VALUE 1.
           05 WS-I              PIC S9(8) COMP.
           05 WS-INPUT-LEN      PIC S9(8) COMP.
           05 WS-COUNT          PIC 9(8)  VALUE 1.
           05 WS-CHAR           PIC X(1).

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE.
           CLOSE INPUT-FILE.

           INSPECT FUNCTION REVERSE(INPUT-RECORD)
               TALLYING WS-I FOR LEADING SPACES.
           COMPUTE WS-INPUT-LEN = FUNCTION LENGTH(INPUT-RECORD) - WS-I.

           SET WS-COL(WS-SANTA-X, WS-SANTA-Y) TO 1.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-INPUT-LEN
               MOVE INPUT-RECORD(WS-I:1) TO WS-CHAR
               IF WS-SANTA-TURN = 1
                   PERFORM UPDATE-SANTA-POS
                   MOVE 0 TO WS-SANTA-TURN
               ELSE
                   PERFORM UPDATE-ROBO-POS
                   MOVE 1 TO WS-SANTA-TURN
               END-IF
           END-PERFORM.

           DISPLAY WS-COUNT.
           STOP RUN.

       UPDATE-SANTA-POS.
           EVALUATE WS-CHAR
               WHEN '^' ADD 1 TO WS-SANTA-Y
               WHEN 'v' SUBTRACT 1 FROM WS-SANTA-Y
               WHEN '>' ADD 1 TO WS-SANTA-X
               WHEN '<' SUBTRACT 1 FROM WS-SANTA-X
           END-EVALUATE.
           IF WS-COL(WS-SANTA-X, WS-SANTA-Y) = 0
               SET WS-COL(WS-SANTA-X, WS-SANTA-Y) TO 1
               ADD 1 TO WS-COUNT
           END-IF.

       UPDATE-ROBO-POS.
           EVALUATE WS-CHAR
               WHEN '^' ADD 1 TO WS-ROBO-Y
               WHEN 'v' SUBTRACT 1 FROM WS-ROBO-Y
               WHEN '>' ADD 1 TO WS-ROBO-X
               WHEN '<' SUBTRACT 1 FROM WS-ROBO-X
           END-EVALUATE.
           IF WS-COL(WS-ROBO-X, WS-ROBO-Y) = 0
               SET WS-COL(WS-ROBO-X, WS-ROBO-Y) TO 1
               ADD 1 TO WS-COUNT
           END-IF.
