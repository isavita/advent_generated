
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIGITAL-PLUMBER.
       AUTHOR. Your Name.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'input.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO 'SYSOUT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(256).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS PIC X(2) VALUE '00'.
           88 END-OF-INPUT VALUE '10'.

       01 WS-CONSTANTS.
           * Max program ID observed in AoC inputs is usually around 2000.
           * Array size is MAX-PROGRAM-ID-VAL + 1 for 0-indexed IDs.
           05 MAX-PROGRAM-ID-VAL   PIC 9(5) VALUE 2000.
           * Max connections per program. Example shows 3, but could be more.
           05 MAX-CONNECTIONS-VAL  PIC 9(3) VALUE 50.

       01 WS-GRAPH-DATA.
           * Adjacency list: WS-PROGRAM-CONNECTIONS(ID + 1)
           05 WS-PROGRAM-CONNECTIONS OCCURS 2001 TIMES.
              10 WS-CONNECTED-COUNT PIC 9(3) VALUE 0.
              10 WS-CONNECTED-TO OCCURS 50 TIMES PIC 9(5) VALUE 0.

       01 WS-VISITED-PROGRAMS.
           * Visited array: WS-VISITED(ID + 1)
           05 WS-VISITED OCCURS 2001 TIMES PIC X(1) VALUE 'N'.
              88 IS-VISITED VALUE 'Y'.
              88 NOT-VISITED VALUE 'N'.

       01 WS-BFS-QUEUE.
           * Queue for BFS: WS-QUEUE-ELEMENTS(index)
           05 WS-QUEUE-ELEMENTS OCCURS 2001 TIMES PIC 9(5).
           05 WS-QUEUE-HEAD PIC 9(5) VALUE 1.
           05 WS-QUEUE-TAIL PIC 9(5) VALUE 0.

       01 WS-BFS-VARS.
           05 WS-CURRENT-PROGRAM-ID PIC 9(5).
           05 WS-NEIGHBOR-ID        PIC 9(5).
           05 WS-GROUP-SIZE         PIC 9(5) VALUE 0.
           05 WS-CONN-IDX           PIC 9(3).

       01 WS-INPUT-PARSING-VARS.
           05 WS-LINE-PROG-ID-STR   PIC X(5).
           05 WS-LINE-ARROW-STR     PIC X(5).
           05 WS-LINE-CONN-LIST-STR PIC X(200).
           05 WS-TEMP-CONN-ID-STR   PIC X(5).
           05 WS-UNSTRING-PTR       PIC 9(3).
           05 WS-CURRENT-PROG-ID-NUM PIC 9(5).
           05 WS-CONNECTED-PROG-ID-NUM PIC 9(5).
           05 WS-MAX-ID-SEEN        PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.
           PERFORM 000-INITIALIZE-PROGRAM.
           PERFORM 100-READ-AND-BUILD-GRAPH
               UNTIL END-OF-INPUT.
           PERFORM 200-BFS-TRAVERSAL.
           PERFORM 900-PRINT-RESULT.
           PERFORM 999-END-PROGRAM.

       000-INITIALIZE-PROGRAM.
           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE.
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'Error opening files. Status: ' WS-FILE-STATUS
               STOP RUN
           END-IF.
           EXIT.

       100-READ-AND-BUILD-GRAPH.
           READ INPUT-FILE INTO INPUT-RECORD
               AT END SET END-OF-INPUT TO TRUE.

           IF NOT END-OF-INPUT THEN
               * Reset unstring pointer for each new line
               MOVE 1 TO WS-UNSTRING-PTR
               * Parse program ID and the rest of the line
               UNSTRING INPUT-RECORD
                   DELIMITED BY ' <-> '
                   INTO WS-LINE-PROG-ID-STR
                        WS-LINE-CONN-LIST-STR
               END-UNSTRING

               * Convert program ID string to numeric
               MOVE FUNCTION NUMVAL(WS-LINE-PROG-ID-STR)
                   TO WS-CURRENT-PROG-ID-NUM

               * Update max ID seen (for potential dynamic sizing, though not used here)
               IF WS-CURRENT-PROG-ID-NUM > WS-MAX-ID-SEEN
                   MOVE WS-CURRENT-PROG-ID-NUM TO WS-MAX-ID-SEEN
               END-IF

               * Parse connected IDs from the list string
               * Loop until pointer exceeds string length or no more valid numbers found
               PERFORM UNTIL WS-UNSTRING-PTR > LENGTH OF WS-LINE-CONN-LIST-STR
                   * Initialize temp string to spaces before each UNSTRING to detect empty results
                   MOVE SPACES TO WS-TEMP-CONN-ID-STR
                   UNSTRING WS-LINE-CONN-LIST-STR
                       DELIMITED BY ',' OR ALL SPACE
                       INTO WS-TEMP-CONN-ID-STR
                       WITH POINTER WS-UNSTRING-PTR
                   END-UNSTRING

                   * Check if a valid number was extracted
                   IF WS-TEMP-CONN-ID-STR NOT = SPACES THEN
                       MOVE FUNCTION NUMVAL(WS-TEMP-CONN-ID-STR)
                           TO WS-CONNECTED-PROG-ID-NUM

                       * Update max ID seen
                       IF WS-CONNECTED-PROG-ID-NUM > WS-MAX-ID-SEEN
                           MOVE WS-CONNECTED-PROG-ID-NUM TO WS-MAX-ID-SEEN
                       END-IF

                       * Add bidirectional connections to the graph
                       PERFORM 110-ADD-CONNECTION
                           USING WS-CURRENT-PROG-ID-NUM WS-CONNECTED-PROG-ID-NUM
                       PERFORM 110-ADD-CONNECTION
                           USING WS-CONNECTED-PROG-ID-NUM WS-CURRENT-PROG-ID-NUM
                   ELSE
                       * If UNSTRING resulted in spaces, it means no more numbers
                       * or only delimiters/spaces left. Exit the loop.
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-IF.
           EXIT.

       110-ADD-CONNECTION SECTION.
       LINKAGE SECTION.
       01 LS-PROG-A PIC 9(5).
       01 LS-PROG-B PIC 9(5).
       PROCEDURE DIVISION USING LS-PROG-A LS-PROG-B.
           * Add LS-PROG-B to LS-PROG-A's connection list
           * Use LS-PROG-A + 1 for 0-indexed program IDs mapping to 1-indexed COBOL array
           ADD 1 TO WS-CONNECTED-COUNT OF WS-PROGRAM-CONNECTIONS(LS-PROG-A + 1).
           IF WS-CONNECTED-COUNT OF WS-PROGRAM-CONNECTIONS(LS-PROG-A + 1)
               > MAX-CONNECTIONS-VAL THEN
               DISPLAY 'Error: Exceeded MAX-CONNECTIONS-VAL for program ' LS-PROG-A
               STOP RUN
           END-IF.
           MOVE LS-PROG-B TO WS-CONNECTED-TO(WS-CONNECTED-COUNT OF WS-PROGRAM-CONNECTIONS(LS-PROG-A + 1))
                               OF WS-PROGRAM-CONNECTIONS(LS-PROG-A + 1).
           EXIT PROGRAM.

       200-BFS-TRAVERSAL.
           * Start BFS from program ID 0
           MOVE 0 TO WS-CURRENT-PROGRAM-ID.

           * Enqueue starting program (0)
           ADD 1 TO WS-QUEUE-TAIL.
           MOVE WS-CURRENT-PROGRAM-ID TO WS-QUEUE-ELEMENTS(WS-QUEUE-TAIL).
           * Mark as visited
           SET IS-VISITED OF WS-VISITED(WS-CURRENT-PROGRAM-ID + 1) TO TRUE.

           * Perform BFS until queue is empty
           PERFORM UNTIL WS-QUEUE-HEAD > WS-QUEUE-TAIL
               * Dequeue current program
               MOVE WS-QUEUE-ELEMENTS(WS-QUEUE-HEAD) TO WS-CURRENT-PROGRAM-ID.
               ADD 1 TO WS-QUEUE-HEAD.

               * Increment group size
               ADD 1 TO WS-GROUP-SIZE.

               * Explore neighbors
               PERFORM VARYING WS-CONN-IDX FROM 1 BY 1
                   UNTIL WS-CONN-IDX > WS-CONNECTED-COUNT OF WS-PROGRAM-CONNECTIONS(WS-CURRENT-PROGRAM-ID + 1)
                   * Get neighbor ID
                   MOVE WS-CONNECTED-TO(WS-CONN-IDX) OF WS-PROGRAM-CONNECTIONS(WS-CURRENT-PROGRAM-ID + 1)
                       TO WS-NEIGHBOR-ID.

                   * If neighbor not visited, mark and enqueue
                   IF NOT IS-VISITED OF WS-VISITED(WS-NEIGHBOR-ID + 1) THEN
                       SET IS-VISITED OF WS-VISITED(WS-NEIGHBOR-ID + 1) TO TRUE.
                       ADD 1 TO WS-QUEUE-TAIL.
                       MOVE WS-NEIGHBOR-ID TO WS-QUEUE-ELEMENTS(WS-QUEUE-TAIL).
                   END-IF
               END-PERFORM
           END-PERFORM.
           EXIT.

       900-PRINT-RESULT.
           MOVE WS-GROUP-SIZE TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           EXIT.

       999-END-PROGRAM.
           CLOSE INPUT-FILE
                 OUTPUT-FILE.
           STOP RUN.
