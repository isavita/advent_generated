
IDENTIFICATION DIVISION.
PROGRAM-ID. LIGHT-SIMULATOR.

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
01 WS-LIGHT-GRID.
   05 WS-ROW OCCURS 1000 TIMES.
      10 WS-COL OCCURS 1000 TIMES PIC S9(5) VALUE 0.

01 WS-TOTAL-BRIGHTNESS PIC S9(18) VALUE 0.

01 WS-FILE-STATUS PIC X(02) VALUE "00".
   88 WS-END-OF-FILE VALUE "10".

01 WS-INPUT-LINE PIC X(256).

01 WS-COMMAND-TYPE PIC X(10).
01 WS-ACTION-TYPE  PIC X(5).

01 WS-COORD-START-X PIC 9(4).
01 WS-COORD-START-Y PIC 9(4).
01 WS-COORD-END-X   PIC 9(4).
01 WS-COORD-END-Y   PIC 9(4).

01 WS-LOOP-ROW-IDX PIC 9(4).
01 WS-LOOP-COL-IDX PIC 9(4).

01 WS-TEMP-PART1 PIC X(10).
01 WS-TEMP-PART2 PIC X(10).
01 WS-TEMP-PART3 PIC X(10).

01 WS-TEMP-COORD-STR1 PIC X(10).
01 WS-TEMP-COORD-STR2 PIC X(10).
01 WS-TEMP-X-STR1 PIC X(4).
01 WS-TEMP-Y-STR1 PIC X(4).
01 WS-TEMP-X-STR2 PIC X(4).
01 WS-TEMP-Y-STR2 PIC X(4).

PROCEDURE DIVISION.
MAIN-LOGIC SECTION.
    PERFORM INITIALIZE-PROGRAM.
    PERFORM PROCESS-FILE.
    PERFORM CALCULATE-TOTAL-BRIGHTNESS.
    PERFORM TERMINATE-PROGRAM.
    STOP RUN.

INITIALIZE-PROGRAM.
    OPEN INPUT INPUT-FILE.
    IF WS-FILE-STATUS NOT = "00"
        DISPLAY "Error opening input.txt: " WS-FILE-STATUS
        STOP RUN
    END-IF.

PROCESS-FILE.
    PERFORM READ-NEXT-RECORD.
    PERFORM UNTIL WS-END-OF-FILE
        PERFORM PARSE-AND-PROCESS-LINE
        PERFORM READ-NEXT-RECORD
    END-PERFORM.

READ-NEXT-RECORD.
    READ INPUT-FILE INTO WS-INPUT-LINE
        AT END SET WS-END-OF-FILE TO TRUE
    END-READ.

PARSE-AND-PROCESS-LINE.
    MOVE SPACES TO WS-TEMP-PART1 WS-TEMP-PART2 WS-TEMP-PART3.
    MOVE SPACES TO WS-TEMP-COORD-STR1 WS-TEMP-COORD-STR2.
    MOVE SPACES TO WS-TEMP-X-STR1 WS-TEMP-Y-STR1 WS-TEMP-X-STR2 WS-TEMP-Y-STR2.

    IF WS-INPUT-LINE(1:6) = 'toggle'
        MOVE 'toggle' TO WS-COMMAND-TYPE
        UNSTRING WS-INPUT-LINE
            DELIMITED BY ALL SPACES
            INTO WS-TEMP-PART1
                 WS-TEMP-COORD-STR1
                 WS-TEMP-PART2
                 WS-TEMP-COORD-STR2
        END-UNSTRING
    ELSE IF WS-INPUT-LINE(1:4) = 'turn'
        MOVE 'turn' TO WS-COMMAND-TYPE
        IF WS-INPUT-LINE(6:2) = 'on'
            MOVE 'on' TO WS-ACTION-TYPE
            UNSTRING WS-INPUT-LINE
                DELIMITED BY ALL SPACES
                INTO WS-TEMP-PART1
                     WS-TEMP-PART2
                     WS-TEMP-COORD-STR1
                     WS-TEMP-PART3
                     WS-TEMP-COORD-STR2
            END-UNSTRING
        ELSE IF WS-INPUT-LINE(6:3) = 'off'
            MOVE 'off' TO WS-ACTION-TYPE
            UNSTRING WS-INPUT-LINE
                DELIMITED BY ALL SPACES
                INTO WS-TEMP-PART1
                     WS-TEMP-PART2
                     WS-TEMP-COORD-STR1
                     WS-TEMP-PART3
                     WS-TEMP-COORD-STR2
            END-UNSTRING
        END-IF
    END-IF.

    UNSTRING WS-TEMP-COORD-STR1
        DELIMITED BY ','
        INTO WS-TEMP-X-STR1 WS-TEMP-Y-STR1
    END-UNSTRING.
    UNSTRING WS-TEMP-COORD-STR2
        DELIMITED BY ','
        INTO WS-TEMP-X-STR2 WS-TEMP-Y-STR2
    END-UNSTRING.

    MOVE FUNCTION NUMVAL(WS-TEMP-X-STR1) TO WS-COORD-START-X.
    MOVE FUNCTION NUMVAL(WS-TEMP-Y-STR1) TO WS-COORD-START-Y.
    MOVE FUNCTION NUMVAL(WS-TEMP-X-STR2) TO WS-COORD-END-X.
    MOVE FUNCTION NUMVAL(WS-TEMP-Y-STR2) TO WS-COORD-END-Y.

    ADD 1 TO WS-COORD-START-X.
    ADD 1 TO WS-COORD-START-Y.
    ADD 1 TO WS-COORD-END-X.
    ADD 1 TO WS-COORD-END-Y.

    PERFORM VARYING WS-LOOP-ROW-IDX FROM WS-COORD-START-X BY 1
        UNTIL WS-LOOP-ROW-IDX > WS-COORD-END-X
        PERFORM VARYING WS-LOOP-COL-IDX FROM WS-COORD-START-Y BY 1
            UNTIL WS-LOOP-COL-IDX > WS-COORD-END-Y
            IF WS-COMMAND-TYPE = 'turn'
                IF WS-ACTION-TYPE = 'on'
                    ADD 1 TO WS-COL(WS-LOOP-ROW-IDX, WS-LOOP-COL-IDX)
                ELSE IF WS-ACTION-TYPE = 'off'
                    SUBTRACT 1 FROM WS-COL(WS-LOOP-ROW-IDX, WS-LOOP-COL-IDX)
                    IF WS-COL(WS-LOOP-ROW-IDX, WS-LOOP-COL-IDX) < 0
                        MOVE 0 TO WS-COL(WS-LOOP-ROW-IDX, WS-LOOP-COL-IDX)
                    END-IF
                END-IF
            ELSE IF WS-COMMAND-TYPE = 'toggle'
                ADD 2 TO WS-COL(WS-LOOP-ROW-IDX, WS-LOOP-COL-IDX)
            END-IF
        END-PERFORM
    END-PERFORM.

CALCULATE-TOTAL-BRIGHTNESS.
    PERFORM VARYING WS-LOOP-ROW-IDX FROM 1 BY 1
        UNTIL WS-LOOP-ROW-IDX > 1000
        PERFORM VARYING WS-LOOP-COL-IDX FROM 1 BY 1
            UNTIL WS-LOOP-COL-IDX > 1000
            ADD WS-COL(WS-LOOP-ROW-IDX, WS-LOOP-COL-IDX) TO WS-TOTAL-BRIGHTNESS
        END-PERFORM
    END-PERFORM.

TERMINATE-PROGRAM.
    CLOSE INPUT-FILE.
    DISPLAY WS-TOTAL-BRIGHTNESS.
