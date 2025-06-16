
IDENTIFICATION DIVISION.
PROGRAM-ID. AOC2015D1.
AUTHOR. Your Name.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT INPUT-FILE ASSIGN TO 'input.txt'
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD INPUT-FILE.
01 INPUT-RECORD PIC X(100000). *> Assumes input line length up to 100,000 characters.
                               *> Adjust if your input is longer.

WORKING-STORAGE SECTION.
01 WS-INPUT-LINE    PIC X(100000).
01 WS-LINE-LENGTH   PIC 9(9) COMP-5.
01 WS-IDX           PIC 9(9) COMP-5.
01 WS-CURRENT-CHAR  PIC X(1).

01 WS-CURRENT-FLOOR PIC S9(9) COMP-5 VALUE 0. *> Tracks Santa's current floor
01 WS-BASEMENT-POS  PIC 9(9) COMP-5 VALUE 0. *> Stores the 1-indexed position for Part 2

01 WS-EOF-FLAG      PIC X(1) VALUE 'N'.
    88 EOF-REACHED VALUE 'Y'.

PROCEDURE DIVISION.
MAIN-LOGIC.
    *> 1. Open the input file and read its content.
    PERFORM OPEN-AND-READ-INPUT.

    *> 2. Process the instructions to find the final floor and first basement entry.
    PERFORM PROCESS-INSTRUCTIONS.

    *> 3. Display the results for both parts.
    PERFORM DISPLAY-RESULTS.

    *> 4. Terminate the program.
    PERFORM TERMINATE-PROGRAM.

OPEN-AND-READ-INPUT.
    OPEN INPUT INPUT-FILE.
    READ INPUT-FILE INTO WS-INPUT-LINE
        AT END SET EOF-REACHED TO TRUE
    END-READ.

    IF EOF-REACHED
        DISPLAY "Error: Input file 'input.txt' is empty or not found."
        PERFORM TERMINATE-PROGRAM
    END-IF.

    *> Determine the actual length of the input line, trimming any potential trailing spaces.
    MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-INPUT-LINE)) TO WS-LINE-LENGTH.
    CLOSE INPUT-FILE.

PROCESS-INSTRUCTIONS.
    *> Iterate through each character of the input line.
    PERFORM VARYING WS-IDX FROM 1 BY 1
        UNTIL WS-IDX > WS-LINE-LENGTH
        MOVE WS-INPUT-LINE(WS-IDX:1) TO WS-CURRENT-CHAR

        *> Update Santa's current floor based on the character.
        IF WS-CURRENT-CHAR = '('
            ADD 1 TO WS-CURRENT-FLOOR
        ELSE IF WS-CURRENT-CHAR = ')'
            SUBTRACT 1 FROM WS-CURRENT-FLOOR
        END-IF

        *> For Part 2: Check if Santa has entered the basement (-1) for the first time.
        *> WS-BASEMENT-POS will be 0 until the first basement entry is found.
        IF WS-CURRENT-FLOOR = -1 AND WS-BASEMENT-POS = 0
            MOVE WS-IDX TO WS-BASEMENT-POS
        END-IF
    END-PERFORM.

DISPLAY-RESULTS.
    DISPLAY "Part 1 - Final Floor: " WS-CURRENT-FLOOR.
    DISPLAY "Part 2 - First Basement Entry Position: " WS-BASEMENT-POS.

TERMINATE-PROGRAM.
    STOP RUN.
