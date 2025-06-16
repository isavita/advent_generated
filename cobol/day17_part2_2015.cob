
IDENTIFICATION DIVISION.
PROGRAM-ID. EGGNOG-CONTAINERS.
AUTHOR. Your Name.
DATE-WRITTEN. 2023-12-17.
REMARKS.
    This program solves the Advent of Code Day 17 puzzle for 2015.
    It reads container capacities from 'input.txt' and finds combinations
    that sum to 150 liters, then reports total combinations and
    combinations using the minimum number of containers.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT INPUT-FILE ASSIGN TO "input.txt"
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT OUTPUT-FILE ASSIGN TO "SYSOUT"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD INPUT-FILE.
01 CONTAINER-CAPACITY-IN PIC 9(3).

FD OUTPUT-FILE.
01 OUTPUT-LINE PIC X(80).

WORKING-STORAGE SECTION.
01 WS-CONSTANTS.
    05 WS-TARGET-EGGNOG PIC 9(3) VALUE 150.
    05 WS-MAX-CONTAINERS PIC 9(2) VALUE 30. *> Max containers expected, adjust if needed

01 WS-CONTAINER-DATA.
    *> OCCURS DEPENDING ON allows dynamic array sizing based on WS-NUM-CONTAINERS
    05 WS-CONTAINERS OCCURS 1 TO WS-MAX-CONTAINERS TIMES
        DEPENDING ON WS-NUM-CONTAINERS
        INDEXED BY WS-IDX.
        10 WS-CAPACITY PIC 9(3).

01 WS-FILE-STATUS PIC X(02).
01 WS-EOF-FLAG    PIC X(01) VALUE 'N'.
    88 EOF-REACHED VALUE 'Y'.

01 WS-NUM-CONTAINERS PIC 9(2) VALUE 0.

01 WS-RESULTS.
    05 WS-TOTAL-COMBINATIONS PIC 9(9) VALUE 0.
    05 WS-MIN-CONTAINERS     PIC 9(2) VALUE 99. *> Initialize with a value higher than any possible count
    05 WS-COMB-FOR-MIN       PIC 9(9) VALUE 0.

01 WS-DISPLAY-VARS.
    05 WS-PART1-OUT PIC Z(8)9.
    05 WS-PART2-MIN-OUT PIC Z(1)9.
    05 WS-PART2-COMB-OUT PIC Z(8)9.

PROCEDURE DIVISION.
MAIN-LOGIC SECTION.
    *> 1. Initialize files
    PERFORM 1000-INITIALIZE.

    *> 2. Read container capacities from input file
    PERFORM 2000-READ-INPUT.

    *> 3. Call the recursive subprogram to find combinations
    *>    Parameters are passed BY VALUE for recursive state and BY REFERENCE for shared results.
    CALL "FIND-COMBINATIONS-RECURSIVE" USING
        BY VALUE 1                 *> Start index (1-based)
        BY VALUE 0                 *> Current sum of capacities
        BY VALUE 0                 *> Current count of containers
        BY REFERENCE WS-NUM-CONTAINERS
        BY REFERENCE WS-CONTAINER-DATA
        BY REFERENCE WS-TARGET-EGGNOG
        BY REFERENCE WS-TOTAL-COMBINATIONS
        BY REFERENCE WS-MIN-CONTAINERS
        BY REFERENCE WS-COMB-FOR-MIN.

    *> 4. Display the calculated results
    PERFORM 4000-DISPLAY-RESULTS.

    *> 5. Clean up and terminate
    PERFORM 9000-CLEANUP.
    STOP RUN.

1000-INITIALIZE.
    OPEN INPUT INPUT-FILE.
    IF WS-FILE-STATUS NOT = "00"
        DISPLAY "Error opening input.txt: " WS-FILE-STATUS
        STOP RUN
    END-IF.
    OPEN OUTPUT OUTPUT-FILE.
    IF WS-FILE-STATUS NOT = "00"
        DISPLAY "Error opening SYSOUT: " WS-FILE-STATUS
        STOP RUN
    END-IF.

2000-READ-INPUT.
    PERFORM UNTIL EOF-REACHED
        READ INPUT-FILE INTO CONTAINER-CAPACITY-IN
            AT END SET EOF-REACHED TO TRUE
            NOT AT END
                ADD 1 TO WS-NUM-CONTAINERS
                IF WS-NUM-CONTAINERS > WS-MAX-CONTAINERS
                    DISPLAY "Error: Too many containers. Max allowed: " WS-MAX-CONTAINERS
                    STOP RUN
                END-IF
                MOVE CONTAINER-CAPACITY-IN TO WS-CAPACITY(WS-NUM-CONTAINERS)
        END-READ
    END-PERFORM.
    CLOSE INPUT-FILE.

4000-DISPLAY-RESULTS.
    MOVE WS-TOTAL-COMBINATIONS TO WS-PART1-OUT.
    STRING "Part 1: Total combinations = " WS-PART1-OUT
           DELIMITED BY SIZE INTO OUTPUT-LINE.
    WRITE OUTPUT-LINE.

    MOVE WS-MIN-CONTAINERS TO WS-PART2-MIN-OUT.
    MOVE WS-COMB-FOR-MIN TO WS-PART2-COMB-OUT.
    STRING "Part 2: Minimum containers = " WS-PART2-MIN-OUT
           ", Combinations for min = " WS-PART2-COMB-OUT
           DELIMITED BY SIZE INTO OUTPUT-LINE.
    WRITE OUTPUT-LINE.

9000-CLEANUP.
    CLOSE OUTPUT-FILE.

END PROGRAM EGGNOG-CONTAINERS.


*> --- Recursive Subprogram Definition ---
IDENTIFICATION DIVISION.
PROGRAM-ID. FIND-COMBINATIONS-RECURSIVE RECURSIVE.
REMARKS.
    This subprogram recursively finds combinations of containers.

DATA DIVISION.
LINKAGE SECTION.
*> Parameters passed BY VALUE (copied for each call)
01 IN-INDEX PIC 9(2).   *> Current container index to consider (1-based)
01 IN-SUM   PIC 9(3).   *> Current sum of capacities of chosen containers
01 IN-COUNT PIC 9(2).   *> Current count of chosen containers

*> Parameters passed BY REFERENCE (shared memory with calling program)
01 LS-NUM-CONTAINERS PIC 9(2).
01 LS-CONTAINER-DATA.
    05 LS-CONTAINERS OCCURS 1 TO 30 TIMES
        DEPENDING ON LS-NUM-CONTAINERS
        INDEXED BY LS-IDX.
        10 LS-CAPACITY PIC 9(3).
01 LS-TARGET-EGGNOG PIC 9(3).
01 LS-TOTAL-COMBINATIONS PIC 9(9).
01 LS-MIN-CONTAINERS     PIC 9(2).
01 LS-COMB-FOR-MIN       PIC 9(9).

PROCEDURE DIVISION USING
    IN-INDEX
    IN-SUM
    IN-COUNT
    LS-NUM-CONTAINERS
    LS-CONTAINER-DATA
    LS-TARGET-EGGNOG
    LS-TOTAL-COMBINATIONS
    LS-MIN-CONTAINERS
    LS-COMB-FOR-MIN.

    *> Base Case 1: Current sum matches the target eggnog amount
    IF IN-SUM = LS-TARGET-EGGNOG
        ADD 1 TO LS-TOTAL-COMBINATIONS
        IF IN-COUNT < LS-MIN-CONTAINERS
            MOVE IN-COUNT TO LS-MIN-CONTAINERS
            MOVE 1 TO LS-COMB-FOR-MIN
        ELSE IF IN-COUNT = LS-MIN-CONTAINERS
            ADD 1 TO LS-COMB-FOR-MIN
        END-IF
        GOBACK *> Return from this recursive call
    END-IF.

    *> Base Case 2: Current sum exceeds target OR no more containers to consider
    IF IN-SUM > LS-TARGET-EGGNOG OR IN-INDEX > LS-NUM-CONTAINERS
        GOBACK *> Return from this recursive call (pruning)
    END-IF.

    *> Recursive Step 1: Include the container at IN-INDEX
    CALL "FIND-COMBINATIONS-RECURSIVE" USING
        BY VALUE IN-INDEX + 1
        BY VALUE IN-SUM + LS-CAPACITY(IN-INDEX)
        BY VALUE IN-COUNT + 1
        BY REFERENCE LS-NUM-CONTAINERS
        BY REFERENCE LS-CONTAINER-DATA
        BY REFERENCE LS-TARGET-EGGNOG
        BY REFERENCE LS-TOTAL-COMBINATIONS
        BY REFERENCE LS-MIN-CONTAINERS
        BY REFERENCE LS-COMB-FOR-MIN.

    *> Recursive Step 2: Exclude the container at IN-INDEX
    CALL "FIND-COMBINATIONS-RECURSIVE" USING
        BY VALUE IN-INDEX + 1
        BY VALUE IN-SUM
        BY VALUE IN-COUNT
        BY REFERENCE LS-NUM-CONTAINERS
        BY REFERENCE LS-CONTAINER-DATA
        BY REFERENCE LS-TARGET-EGGNOG
        BY REFERENCE LS-TOTAL-COMBINATIONS
        BY REFERENCE LS-MIN-CONTAINERS
        BY REFERENCE LS-COMB-FOR-MIN.

    GOBACK. *> Return from this recursive call

END PROGRAM FIND-COMBINATIONS-RECURSIVE.
