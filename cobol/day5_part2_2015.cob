
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NICE-STRINGS.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD      PIC X(80).
       WORKING-STORAGE SECTION.
       01  NICE-COUNT        PIC 9(9) VALUE 0.
       01  CURRENT-STRING    PIC X(80).
       01  STRING-LENGTH     PIC 9(2).
       01  I                 PIC 9(2).
       01  J                 PIC 9(2).
       01  CHAR-PAIR         PIC XX.
       01  PAIR-FOUND        PIC X VALUE 'N'.
       01  LETTER-MATCH      PIC X VALUE 'N'.
       01  IS-NICE           PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE.
           PERFORM READ-AND-PROCESS UNTIL END-OF-FILE.
           CLOSE INPUT-FILE.
           DISPLAY NICE-COUNT.
           STOP RUN.

       READ-AND-PROCESS.
           READ INPUT-FILE
               AT END MOVE 'Y' TO END-OF-FILE.
           IF END-OF-FILE = 'N' THEN
               MOVE INPUT-RECORD TO CURRENT-STRING
               INSPECT CURRENT-STRING REPLACING ALL SPACES WITH LOW-VALUE
               MOVE LENGTH(CURRENT-STRING) TO STRING-LENGTH
               MOVE 'N' TO IS-NICE

               IF STRING-LENGTH >= 4 THEN
                   MOVE 'N' TO PAIR-FOUND
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > STRING-LENGTH - 1
                       MOVE CURRENT-STRING(I:2) TO CHAR-PAIR
                       PERFORM VARYING J FROM 1 BY 1 UNTIL J > STRING-LENGTH - 1
                           IF CHAR-PAIR = CURRENT-STRING(J:2) AND I <> J
                               MOVE 'Y' TO PAIR-FOUND
                               EXIT PERFORM
                           END-IF
                       END-PERFORM
                       IF PAIR-FOUND = 'Y'
                           EXIT PERFORM
                       END-IF
                   END-PERFORM

                   IF PAIR-FOUND = 'Y' THEN
                       MOVE 'N' TO LETTER-MATCH
                       PERFORM VARYING I FROM 1 BY 1 UNTIL I > STRING-LENGTH - 2
                           IF CURRENT-STRING(I:1) = CURRENT-STRING(I+2:1)
                               MOVE 'Y' TO LETTER-MATCH
                               EXIT PERFORM
                           END-IF
                       END-PERFORM

                       IF LETTER-MATCH = 'Y'
                           MOVE 'Y' TO IS-NICE
                       END-IF
                   END-IF
               END-IF

               IF IS-NICE = 'Y'
                   ADD 1 TO NICE-COUNT
               END-IF
           END-IF.

       END-OF-FILE       PIC X VALUE 'N'.
       END PROGRAM NICE-STRINGS.
