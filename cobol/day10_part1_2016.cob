
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOT-CHIP-SORTER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'input.txt'.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  RECORD-DATA         PIC X(128).

       WORKING-STORAGE SECTION.
       01  BOT-DATA.
           05  BOT-RECORD OCCURS 256 TIMES.
               10  LOW-TYPE        PIC 9.
               10  LOW-ID          PIC 9(3).
               10  HIGH-TYPE       PIC 9.
               10  HIGH-ID         PIC 9(3).
               10  CHIPS OCCURS 2 TIMES PIC 9(3).
               10  CHIP-COUNT      PIC 9.

       01  OUTPUT-DATA.
           05  OUTPUT-VALUE OCCURS 256 TIMES PIC 9(3).

       01  WS-FILE-STATUS      PIC XX.
       01  WS-LINE             PIC X(128).
       01  WS-VALUE            PIC 9(3).
       01  WS-BOT-ID           PIC 9(3).
       01  WS-LOW-ID           PIC 9(3).
       01  WS-HIGH-ID          PIC 9(3).
       01  WS-LOW-TYPE-STR     PIC X(10).
       01  WS-HIGH-TYPE-STR    PIC X(10).
       01  WS-LOW-CHIP         PIC 9(3).
       01  WS-HIGH-CHIP        PIC 9(3).
       01  WS-TARGET-BOT       PIC 9(3) VALUE -1.
       01  WS-MOVED            PIC 1.
       01  WS-I                PIC 9(3).
       01  WS-J                PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-BOTS.
           PERFORM READ-AND-PARSE-INPUT.
           PERFORM PROCESS-BOTS UNTIL WS-MOVED = 0.
           DISPLAY WS-TARGET-BOT.
           STOP RUN.

       INITIALIZE-BOTS.
           MOVE SPACES TO BOT-RECORD.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 256
               MOVE 0 TO CHIP-COUNT(WS-I)
           END-PERFORM.

       READ-AND-PARSE-INPUT.
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL WS-FILE-STATUS NOT = '00'
               READ INPUT-FILE RECORD INTO WS-LINE
               IF WS-FILE-STATUS = '00'
                   IF WS-LINE(1:5) = 'value'
                       INSPECT WS-LINE REPLACING ALL ' goes to bot ' BY SPACE
                       INSPECT WS-LINE REPLACING ALL 'value ' BY SPACE
                       MOVE WS-LINE TO WS-VALUE WS-BOT-ID
                       ADD 1 TO CHIP-COUNT(WS-BOT-ID)
                       MOVE WS-VALUE TO CHIPS(WS-BOT-ID, CHIP-COUNT(WS-BOT-ID))
                   ELSE
                       INSPECT WS-LINE REPLACING ALL ' bot ' BY SPACE
                       INSPECT WS-LINE REPLACING ALL ' gives low to ' BY SPACE
                       INSPECT WS-LINE REPLACING ALL ' and high to ' BY SPACE
                       INSPECT WS-LINE REPLACING ALL ' output ' BY '1'
                       INSPECT WS-LINE REPLACING ALL ' bot ' BY '0'
                       MOVE WS-LINE TO WS-BOT-ID WS-LOW-TYPE-STR WS-LOW-ID WS-HIGH-TYPE-STR WS-HIGH-ID
                       MOVE WS-BOT-ID TO WS-I
                       MOVE WS-LOW-TYPE-STR TO LOW-TYPE(WS-I)
                       MOVE WS-LOW-ID TO LOW-ID(WS-I)
                       MOVE WS-HIGH-TYPE-STR TO HIGH-TYPE(WS-I)
                       MOVE WS-HIGH-ID TO HIGH-ID(WS-I)
                   END-IF
               END-IF
           END-PERFORM.
           CLOSE INPUT-FILE.

       PROCESS-BOTS.
           MOVE 0 TO WS-MOVED.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 256
               IF CHIP-COUNT(WS-I) = 2
                   MOVE 1 TO WS-MOVED
                   IF CHIPS(WS-I, 1) < CHIPS(WS-I, 2)
                       MOVE CHIPS(WS-I, 1) TO WS-LOW-CHIP
                       MOVE CHIPS(WS-I, 2) TO WS-HIGH-CHIP
                   ELSE
                       MOVE CHIPS(WS-I, 2) TO WS-LOW-CHIP
                       MOVE CHIPS(WS-I, 1) TO WS-HIGH-CHIP
                   END-IF

                   IF WS-LOW-CHIP = 17 AND WS-HIGH-CHIP = 61
                       MOVE WS-I TO WS-TARGET-BOT
                   END-IF

                   IF LOW-TYPE(WS-I) = 0
                       ADD 1 TO CHIP-COUNT(LOW-ID(WS-I))
                       MOVE WS-LOW-CHIP TO CHIPS(LOW-ID(WS-I), CHIP-COUNT(LOW-ID(WS-I)))
                   ELSE
                       MOVE WS-LOW-CHIP TO OUTPUT-VALUE(LOW-ID(WS-I))
                   END-IF

                   IF HIGH-TYPE(WS-I) = 0
                       ADD 1 TO CHIP-COUNT(HIGH-ID(WS-I))
                       MOVE WS-HIGH-CHIP TO CHIPS(HIGH-ID(WS-I), CHIP-COUNT(HIGH-ID(WS-I)))
                   ELSE
                       MOVE WS-HIGH-CHIP TO OUTPUT-VALUE(HIGH-ID(WS-I))
                   END-IF
                   MOVE 0 TO CHIP-COUNT(WS-I)
               END-IF
           END-PERFORM.
