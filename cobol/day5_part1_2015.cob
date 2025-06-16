
IDENTIFICATION DIVISION.
PROGRAM-ID. DAY05.
AUTHOR. Your Name.
DATE-WRITTEN. 2023-12-05.
REMARKS.
    This program solves Advent of Code 2015, Day 5.
    It counts "nice" strings based on three rules:
    1. At least three vowels (a, e, i, o, u).
    2. At least one letter that appears twice in a row (e.g., 'xx').
    3. Does not contain 'ab', 'cd', 'pq', or 'xy'.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT INPUT-FILE ASSIGN TO 'input.txt'
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT PRINT-FILE ASSIGN TO 'stdout'  *> 'stdout' for GnuCOBOL, 'SYS$OUTPUT' for Micro Focus
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD INPUT-FILE.
01 FILLER PIC X(255). *> Defines the maximum record length for the file.
                      *> Actual data is read into WS-CURRENT-STRING.

FD PRINT-FILE.
01 PRINT-RECORD PIC X(255).

WORKING-STORAGE SECTION.
01 WS-FILE-STATUS PIC X(02).
   88 WS-END-OF-FILE VALUE '10'.

01 WS-COUNTERS.
   05 WS-NICE-STRINGS-COUNT PIC 9(05) VALUE 0.
   05 WS-VOWEL-COUNT        PIC 9(02) VALUE 0.

01 WS-STRING-INFO.
   05 WS-CURRENT-STRING     PIC X(255). *> Holds the string read from the input file.
   05 WS-STRING-LENGTH      PIC 9(03).  *> Length of the current string.
   05 WS-IDX                PIC 9(03).  *> Loop index for string iteration.

01 WS-FLAGS.
   05 WS-HAS-DOUBLE-LETTER  PIC X(01) VALUE 'N'.
      88 HAS-DOUBLE-LETTER  VALUE 'Y'.
   05 WS-HAS-DISALLOWED-PAIR PIC X(01) VALUE 'N'.
      88 HAS-DISALLOWED-PAIR VALUE 'Y'.

01 WS-CHAR-PAIRS.
   05 WS-CURRENT-CHAR       PIC X(01).  *> Current character in the string.
   05 WS-NEXT-CHAR          PIC X(01).  *> Next character in the string.
   05 WS-TWO-CHARS          PIC X(02).  *> Two-character substring for pair checks.

PROCEDURE DIVISION.
MAIN-LOGIC SECTION.
    *> Main entry point for the program.
    PERFORM INITIALIZE-PROGRAM.
    PERFORM PROCESS-INPUT-FILE.
    PERFORM TERMINATE-PROGRAM.
    STOP RUN.

INITIALIZE-PROGRAM.
    *> Opens the input and output files.
    OPEN INPUT INPUT-FILE.
    IF WS-FILE-STATUS NOT = '00'
        DISPLAY 'Error opening input.txt: ' WS-FILE-STATUS
        STOP RUN
    END-IF.
    OPEN OUTPUT PRINT-FILE.
    IF WS-FILE-STATUS NOT = '00'
        DISPLAY 'Error opening stdout: ' WS-FILE-STATUS
        STOP RUN
    END-IF.

PROCESS-INPUT-FILE.
    *> Reads and processes each line from the input file until end-of-file.
    PERFORM READ-INPUT-RECORD.
    PERFORM UNTIL WS-END-OF-FILE
        PERFORM CHECK-STRING-NICENESS
        PERFORM READ-INPUT-RECORD
    END-PERFORM.

READ-INPUT-RECORD.
    *> Reads a single record (line) from the input file into WS-CURRENT-STRING.
    READ INPUT-FILE INTO WS-CURRENT-STRING
        AT END SET WS-END-OF-FILE TO TRUE
    END-READ.

CHECK-STRING-NICENESS.
    *> Initializes flags and counters for the current string.
    *> Performs a single pass to check all three niceness rules.
    INITIALIZE WS-VOWEL-COUNT WS-HAS-DOUBLE-LETTER WS-HAS-DISALLOWED-PAIR.
    MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-CURRENT-STRING)) TO WS-STRING-LENGTH.

    IF WS-STRING-LENGTH = 0
        EXIT PARAGRAPH *> Skip empty lines
    END-IF.

    *> Iterate through the string character by character.
    PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-STRING-LENGTH
        MOVE WS-CURRENT-STRING(WS-IDX:1) TO WS-CURRENT-CHAR

        *> Rule 1: Count vowels
        EVALUATE WS-CURRENT-CHAR
            WHEN 'a'
            WHEN 'e'
            WHEN 'i'
            WHEN 'o'
            WHEN 'u'
                ADD 1 TO WS-VOWEL-COUNT
        END-EVALUATE.

        *> Rules 2 & 3 require looking at the next character/pair.
        *> These checks are only possible if there's a next character.
        IF WS-IDX < WS-STRING-LENGTH
            MOVE WS-CURRENT-STRING(WS-IDX + 1:1) TO WS-NEXT-CHAR
            MOVE WS-CURRENT-STRING(WS-IDX:2) TO WS-TWO-CHARS

            *> Rule 2: Check for a double letter (e.g., 'xx')
            *> Only set the flag if not already found to avoid redundant checks.
            IF NOT HAS-DOUBLE-LETTER AND WS-CURRENT-CHAR = WS-NEXT-CHAR
                SET HAS-DOUBLE-LETTER TO TRUE
            END-IF.

            *> Rule 3: Check for disallowed substrings ('ab', 'cd', 'pq', 'xy')
            *> Only set the flag if not already found.
            IF NOT HAS-DISALLOWED-PAIR
                EVALUATE WS-TWO-CHARS
                    WHEN 'ab'
                    WHEN 'cd'
                    WHEN 'pq'
                    WHEN 'xy'
                        SET HAS-DISALLOWED-PAIR TO TRUE
                END-EVALUATE
            END-IF
        END-IF
    END-PERFORM.

    *> Determine if the string is nice based on all three rules.
    IF WS-VOWEL-COUNT >= 3 AND HAS-DOUBLE-LETTER AND NOT HAS-DISALLOWED-PAIR
        ADD 1 TO WS-NICE-STRINGS-COUNT
    END-IF.

TERMINATE-PROGRAM.
    *> Closes files and prints the final count.
    CLOSE INPUT-FILE.
    CLOSE PRINT-FILE.
    MOVE WS-NICE-STRINGS-COUNT TO PRINT-RECORD.
    WRITE PRINT-RECORD.
    *> Display to console for immediate feedback (optional, but useful).
    DISPLAY 'Total nice strings: ' WS-NICE-STRINGS-COUNT.
