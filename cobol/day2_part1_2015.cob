
IDENTIFICATION DIVISION.
PROGRAM-ID. PRESENT-PAPER.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT INPUT-FILE ASSIGN TO "input.txt"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD INPUT-FILE.
01 INPUT-RECORD PIC X(256).

WORKING-STORAGE SECTION.
01 WS-EOF-FLAG PIC X VALUE 'N'.
   88 WS-EOF VALUE 'Y'.

01 WS-TOTAL-PAPER PIC 9(12) COMP-5 VALUE 0.

01 WS-DIMENSIONS.
   05 WS-L PIC 9(6) COMP-5.
   05 WS-W PIC 9(6) COMP-5.
   05 WS-H PIC 9(6) COMP-5.

01 WS-SIDES.
   05 WS-SIDE1 PIC 9(12) COMP-5.
   05 WS-SIDE2 PIC 9(12) COMP-5.
   05 WS-SIDE3 PIC 9(12) COMP-5.

01 WS-MIN-SIDE PIC 9(12) COMP-5.
01 WS-CURRENT-PACKAGE-PAPER PIC 9(12) COMP-5.

01 WS-TEMP-L PIC X(10).
01 WS-TEMP-W PIC X(10).
01 WS-TEMP-H PIC X(10).

PROCEDURE DIVISION.
MAIN-LOGIC.
    OPEN INPUT INPUT-FILE

    PERFORM UNTIL WS-EOF
        READ INPUT-FILE INTO INPUT-RECORD
            AT END SET WS-EOF TO TRUE
            NOT AT END PERFORM PROCESS-RECORD
        END-READ
    END-PERFORM

    CLOSE INPUT-FILE

    DISPLAY WS-TOTAL-PAPER

    STOP RUN.

PROCESS-RECORD.
    MOVE SPACES TO WS-TEMP-L WS-TEMP-W WS-TEMP-H

    UNSTRING INPUT-RECORD DELIMITED BY "x"
        INTO WS-TEMP-L
             WS-TEMP-W
             WS-TEMP-H
    END-UNSTRING

    MOVE FUNCTION NUMVAL(WS-TEMP-L) TO WS-L
    MOVE FUNCTION NUMVAL(WS-TEMP-W) TO WS-W
    MOVE FUNCTION NUMVAL(WS-TEMP-H) TO WS-H

    COMPUTE WS-SIDE1 = WS-L * WS-W
    COMPUTE WS-SIDE2 = WS-W * WS-H
    COMPUTE WS-SIDE3 = WS-H * WS-L

    MOVE WS-SIDE1 TO WS-MIN-SIDE
    IF WS-SIDE2 < WS-MIN-SIDE THEN
        MOVE WS-SIDE2 TO WS-MIN-SIDE
    END-IF
    IF WS-SIDE3 < WS-MIN-SIDE THEN
        MOVE WS-SIDE3 TO WS-MIN-SIDE
    END-IF

    COMPUTE WS-CURRENT-PACKAGE-PAPER = (2 * WS-SIDE1) + (2 * WS-SIDE2) + (2 * WS-SIDE3) + WS-MIN-SIDE

    ADD WS-CURRENT-PACKAGE-PAPER TO WS-TOTAL-PAPER.
