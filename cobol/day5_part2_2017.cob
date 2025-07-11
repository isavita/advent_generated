
IDENTIFICATION DIVISION.
PROGRAM-ID. JUMP-PUZZLE.

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
01  WS-EOF                 PIC X(01) VALUE 'N'.
    88  END-OF-FILE                  VALUE 'Y'.

01  WS-JUMPS-TABLE.
    05  WS-JUMP OCCURS 5000 TIMES
        INDEXED BY JUMP-IDX          PIC S9(09) COMP-5.

01  WS-CURRENT-JUMP-COUNT  PIC 9(04) VALUE 0.

01  WS-INDEX               PIC S9(09) COMP-5 VALUE 0.
01  WS-STEPS               PIC S9(09) COMP-5 VALUE 0.
01  WS-OFFSET              PIC S9(09) COMP-5.

PROCEDURE DIVISION.
MAIN-LOGIC SECTION.
    PERFORM INITIALIZE-PROGRAM
    PERFORM READ-INPUT-FILE
    PERFORM PROCESS-JUMPS
    PERFORM TERMINATE-PROGRAM.

INITIALIZE-PROGRAM SECTION.
    OPEN INPUT INPUT-FILE.

READ-INPUT-FILE SECTION.
    PERFORM VARYING JUMP-IDX FROM 1 BY 1
        UNTIL END-OF-FILE OR JUMP-IDX > 5000
        READ INPUT-FILE INTO INPUT-RECORD
            AT END
                SET END-OF-FILE TO TRUE
            NOT AT END
                MOVE INPUT-RECORD TO WS-JUMP(JUMP-IDX)
                ADD 1 TO WS-CURRENT-JUMP-COUNT
        END-READ
    END-PERFORM.
    CLOSE INPUT-FILE.

PROCESS-JUMPS SECTION.
    PERFORM UNTIL WS-INDEX < 0 OR WS-INDEX >= WS-CURRENT-JUMP-COUNT
        COMPUTE WS-OFFSET = WS-JUMP(WS-INDEX + 1)
        IF WS-OFFSET < 3
            ADD 1 TO WS-JUMP(WS-INDEX + 1)
        ELSE
            SUBTRACT 1 FROM WS-JUMP(WS-INDEX + 1)
        END-IF
        ADD WS-OFFSET TO WS-INDEX
        ADD 1 TO WS-STEPS
    END-PERFORM.

TERMINATE-PROGRAM SECTION.
    DISPLAY WS-STEPS.
    STOP RUN.
