
IDENTIFICATION DIVISION.
PROGRAM-ID. FIREWALL-RULES.
AUTHOR. Your Name.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT INPUT-FILE ASSIGN TO 'input.txt'
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD INPUT-FILE.
01 INPUT-RECORD PIC X(21).

WORKING-STORAGE SECTION.
01 WS-EOF-FLAG           PIC X(01) VALUE 'N'.
   88 WS-EOF                     VALUE 'Y'.

* Define a table to store IP ranges.
* OCCURS 10000 TIMES is an estimate for typical Advent of Code input sizes.
* ASCENDING KEY and INDEXED BY are used for sorting and iteration.
01 WS-RANGE-TABLE.
   05 WS-RANGE-ENTRY OCCURS 10000 TIMES
      ASCENDING KEY WS-RANGE-START WS-RANGE-END
      INDEXED BY WS-RANGE-IDX.
      10 WS-RANGE-START PIC 9(10).
      10 WS-RANGE-END   PIC 9(10).

01 WS-RANGE-COUNT        PIC 9(05) VALUE 0.
01 WS-CURRENT-LINE       PIC X(21).
01 WS-DASH-POS           PIC 9(02).
01 WS-START-STR          PIC X(10).
01 WS-END-STR            PIC X(10).

01 WS-LOWEST-ALLOWED-IP  PIC 9(10) VALUE 0.
01 WS-TOTAL-ALLOWED-IPS  PIC 9(10) VALUE 0.
01 WS-FOUND-PART1-ANSWER PIC X(01) VALUE 'N'.
   88 PART1-ANSWER-FOUND         VALUE 'Y'.

* WS-CURRENT-IP-CHECK tracks the next IP that could potentially be allowed.
01 WS-CURRENT-IP-CHECK   PIC 9(10).
* The maximum possible 32-bit unsigned integer value.
01 WS-MAX-IP-VALUE       PIC 9(10) VALUE 4294967295.

PROCEDURE DIVISION.
MAIN-LOGIC.
    PERFORM 1000-INITIALIZE.
    PERFORM 2000-READ-INPUT-RANGES.
    PERFORM 3000-SORT-RANGES.
    PERFORM 4000-MERGE-AND-CALCULATE.
    PERFORM 5000-DISPLAY-RESULTS.
    PERFORM 9000-TERMINATE.

1000-INITIALIZE.
* Opens the input file and initializes working variables.
    OPEN INPUT INPUT-FILE.
    MOVE 0 TO WS-RANGE-COUNT.
    MOVE 0 TO WS-LOWEST-ALLOWED-IP.
    MOVE 0 TO WS-TOTAL-ALLOWED-IPS.
    MOVE 'N' TO WS-FOUND-PART1-ANSWER.
    MOVE 0 TO WS-CURRENT-IP-CHECK.

2000-READ-INPUT-RANGES.
* Reads each line from input.txt, parses the start and end IPs,
* and stores them in the WS-RANGE-TABLE.
    PERFORM VARYING WS-RANGE-IDX FROM 1 BY 1
        UNTIL WS-EOF OR WS-RANGE-IDX > 10000
        READ INPUT-FILE INTO WS-CURRENT-LINE
            AT END
                SET WS-EOF TO TRUE
            NOT AT END
                ADD 1 TO WS-RANGE-COUNT
                * UNSTRING parses the "START-END" format.
                UNSTRING WS-CURRENT-LINE DELIMITED BY '-'
                    INTO WS-START-STR, WS-END-STR
                * NUMVAL converts the string parts to numeric values.
                MOVE FUNCTION NUMVAL(WS-START-STR) TO WS-RANGE-START(WS-RANGE-IDX)
                MOVE FUNCTION NUMVAL(WS-END-STR)   TO WS-RANGE-END(WS-RANGE-IDX)
        END-READ
    END-PERFORM.
    CLOSE INPUT-FILE.

3000-SORT-RANGES.
* Sorts the WS-RANGE-ENTRY table in place.
* This is a GnuCOBOL extension for sorting internal tables.
    IF WS-RANGE-COUNT > 0 THEN
        SORT WS-RANGE-ENTRY
            ON ASCENDING KEY WS-RANGE-START
            ON ASCENDING KEY WS-RANGE-END
        END-SORT.
    END-IF.

4000-MERGE-AND-CALCULATE.
* Initializes variables for the merging process.
    MOVE 0 TO WS-CURRENT-IP-CHECK.
    MOVE 0 TO WS-TOTAL-ALLOWED-IPS.
    MOVE 'N' TO WS-FOUND-PART1-ANSWER.

    IF WS-RANGE-COUNT > 0 THEN
        * Iterate through each sorted range.
        PERFORM VARYING WS-RANGE-IDX FROM 1 BY 1
            UNTIL WS-RANGE-IDX > WS-RANGE-COUNT

            * If the current range's start IP is greater than WS-CURRENT-IP-CHECK,
            * it means there's a gap of allowed IPs.
            IF WS-RANGE-START(WS-RANGE-IDX) > WS-CURRENT-IP-CHECK THEN
                * If Part 1 answer hasn't been found yet, this gap's start is the answer.
                IF NOT PART1-ANSWER-FOUND THEN
                    MOVE WS-CURRENT-IP-CHECK TO WS-LOWEST-ALLOWED-IP
                    SET PART1-ANSWER-FOUND TO TRUE
                END-IF
                * Add the length of this allowed gap to the total.
                ADD (WS-RANGE-START(WS-RANGE-IDX) - WS-CURRENT-IP-CHECK)
                    TO WS-TOTAL-ALLOWED-IPS
            END-IF

            * Advance WS-CURRENT-IP-CHECK past the current blocked range.
            * This handles overlapping ranges by ensuring WS-CURRENT-IP-CHECK
            * always moves to the highest possible 'end + 1' encountered so far.
            IF WS-RANGE-END(WS-RANGE-IDX) + 1 > WS-CURRENT-IP-CHECK THEN
                MOVE WS-RANGE-END(WS-RANGE-IDX) + 1 TO WS-CURRENT-IP-CHECK
            END-IF
        END-PERFORM.

        * After processing all ranges, check for any remaining allowed IPs
        * from WS-CURRENT-IP-CHECK up to WS-MAX-IP-VALUE.
        IF WS-CURRENT-IP-CHECK <= WS-MAX-IP-VALUE THEN
            * If Part 1 answer still not found (e.g., no blocked ranges started at 0),
            * then WS-CURRENT-IP-CHECK is the lowest allowed IP.
            IF NOT PART1-ANSWER-FOUND THEN
                MOVE WS-CURRENT-IP-CHECK TO WS-LOWEST-ALLOWED-IP
                SET PART1-ANSWER-FOUND TO TRUE
            END-IF
            * Add the remaining allowed IPs to the total.
            ADD (WS-MAX-IP-VALUE - WS-CURRENT-IP-CHECK + 1) TO WS-TOTAL-ALLOWED-IPS
        END-IF
    ELSE
        * If there are no ranges in the input, all IPs are allowed.
        MOVE 0 TO WS-LOWEST-ALLOWED-IP.
        ADD WS-MAX-IP-VALUE 1 TO WS-TOTAL-ALLOWED-IPS.
    END-IF.

5000-DISPLAY-RESULTS.
* Prints the calculated results to standard output.
    DISPLAY "Lowest-valued IP not blocked: " WS-LOWEST-ALLOWED-IP.
    DISPLAY "Total IPs allowed: " WS-TOTAL-ALLOWED-IPS.

9000-TERMINATE.
* Ends the program execution.
    STOP RUN.
