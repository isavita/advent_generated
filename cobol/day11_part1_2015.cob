
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CorporatePassword.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  InputFile.
       01  InputRecord           PIC X(8).

       WORKING-STORAGE SECTION.
       01  WS-PASSWORD           PIC X(8).
       01  FILLER REDEFINES WS-PASSWORD.
           05 WS-PASS-CHARS      PIC X OCCURS 8.

       01  WS-FLAGS.
           05 WS-IS-VALID-FLAG   PIC 9 VALUE 0.
              88 IS-VALID        VALUE 1.

       01  WS-VALIDATION-VARS.
           05 WS-STRAIGHT-FOUND  PIC 9.
              88 STRAIGHT-FOUND  VALUE 1.
           05 WS-PAIR-COUNT      PIC 99.
           05 WS-PREV-CHAR       PIC X.
           05 WS-PREV2-CHAR      PIC X.

       01  I                     PIC 9.
       01  WS-ORD-VAL            PIC 999.

       PROCEDURE DIVISION.
       main.
           OPEN INPUT InputFile.
           READ InputFile INTO WS-PASSWORD.
           CLOSE InputFile.

           PERFORM UNTIL IS-VALID
               PERFORM Increment-Password
               PERFORM Is-Valid-Password
           END-PERFORM.

           DISPLAY WS-PASSWORD.
           STOP RUN.

       Increment-Password.
           PERFORM VARYING I FROM 8 BY -1 UNTIL I < 1
               IF WS-PASS-CHARS(I) = 'z'
                   MOVE 'a' TO WS-PASS-CHARS(I)
               ELSE
                   COMPUTE WS-ORD-VAL = FUNCTION ORD(WS-PASS-CHARS(I)) + 1
                   MOVE FUNCTION CHAR(WS-ORD-VAL) TO WS-PASS-CHARS(I)
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       Is-Valid-Password.
           MOVE 0 TO WS-IS-VALID-FLAG, WS-STRAIGHT-FOUND, WS-PAIR-COUNT.
           MOVE SPACE TO WS-PREV-CHAR, WS-PREV2-CHAR.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
               EVALUATE WS-PASS-CHARS(I)
                   WHEN 'i' WHEN 'o' WHEN 'l'
                       EXIT PARAGRAPH
               END-EVALUATE

               IF WS-PASS-CHARS(I) = WS-PREV-CHAR AND
                  WS-PASS-CHARS(I) NOT = WS-PREV2-CHAR
                   ADD 1 TO WS-PAIR-COUNT
                   MOVE SPACE TO WS-PREV-CHAR
               ELSE IF WS-PASS-CHARS(I) = WS-PREV-CHAR
                   MOVE SPACE TO WS-PREV-CHAR
               ELSE
                   MOVE WS-PREV-CHAR TO WS-PREV2-CHAR
                   MOVE WS-PASS-CHARS(I) TO WS-PREV-CHAR
               END-IF

               IF I > 2
                   IF FUNCTION ORD(WS-PASS-CHARS(I)) =
                      FUNCTION ORD(WS-PASS-CHARS(I - 1)) + 1 AND
                      FUNCTION ORD(WS-PASS-CHARS(I - 1)) =
                      FUNCTION ORD(WS-PASS-CHARS(I - 2)) + 1
                       MOVE 1 TO WS-STRAIGHT-FOUND
                   END-IF
               END-IF
           END-PERFORM.

           IF STRAIGHT-FOUND AND WS-PAIR-COUNT >= 2
               MOVE 1 TO WS-IS-VALID-FLAG
           END-IF.
