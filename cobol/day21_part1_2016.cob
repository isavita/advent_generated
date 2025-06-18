
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCRAMBLER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD           PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-PASSWORD            PIC X(8) VALUE "abcdefgh".
       01  WS-PASSWORD-LEN        PIC 9(2) VALUE 8.

       01  WS-OP-LINE             PIC X(80).
       01  WS-OP-TYPE             PIC X(10).
       01  WS-PARAM1              PIC X(10).
       01  WS-PARAM2              PIC X(10).
       01  WS-PARAM3              PIC X(10).
       01  WS-PARAM4              PIC X(10).
       01  WS-PARAM5              PIC X(10).
       01  WS-PARAM6              PIC X(10).

       01  WS-NUM1                PIC 9(2).
       01  WS-NUM2                PIC 9(2).
       01  WS-CHAR1               PIC X(1).
       01  WS-CHAR2               PIC X(1).
       01  WS-STEPS               PIC 9(2).
       01  WS-INDEX               PIC 9(2).

       01  WS-EOF-FLAG            PIC X(1) VALUE 'N'.
           88 EOF-REACHED                   VALUE 'Y'.

       01  WS-TEMP-CHAR           PIC X(1).
       01  WS-TEMP-NUM            PIC 9(2).
       01  WS-TEMP-PASSWORD       PIC X(8).
       01  WS-PASSWORD-7-CHARS    PIC X(7).

       01  WS-SUBSTRING           PIC X(8).
       01  WS-REVERSED-SUBSTRING  PIC X(8).
       01  WS-SUBSTRING-LEN       PIC 9(2).

       01  WS-I                   PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-LOGIC SECTION.
           OPEN INPUT INPUT-FILE.

           PERFORM UNTIL EOF-REACHED
               READ INPUT-FILE INTO WS-OP-LINE
                   AT END SET EOF-REACHED TO TRUE
                   NOT AT END PERFORM PROCESS-OPERATION
               END-READ
           END-PERFORM.

           CLOSE INPUT-FILE.
           DISPLAY WS-PASSWORD.
           STOP RUN.

       PROCESS-OPERATION SECTION.
           UNSTRING WS-OP-LINE DELIMITED BY SPACE OR ALL SPACE
               INTO WS-OP-TYPE WS-PARAM1 WS-PARAM2 WS-PARAM3
                    WS-PARAM4 WS-PARAM5 WS-PARAM6
               ON OVERFLOW CONTINUE
           END-UNSTRING.

           EVALUATE WS-OP-TYPE
               WHEN "swap"
                   EVALUATE WS-PARAM1
                       WHEN "position"
                           MOVE WS-PARAM2 TO WS-NUM1
                           MOVE WS-PARAM5 TO WS-NUM2
                           PERFORM SWAP-POSITION
                       WHEN "letter"
                           MOVE WS-PARAM2 TO WS-CHAR1
                           MOVE WS-PARAM5 TO WS-CHAR2
                           PERFORM SWAP-LETTER
                   END-EVALUATE
               WHEN "rotate"
                   EVALUATE WS-PARAM1
                       WHEN "left"
                           MOVE WS-PARAM2 TO WS-STEPS
                           PERFORM ROTATE-LEFT
                       WHEN "right"
                           MOVE WS-PARAM2 TO WS-STEPS
                           PERFORM ROTATE-RIGHT
                       WHEN "based"
                           MOVE WS-PARAM6 TO WS-CHAR1
                           PERFORM ROTATE-BASED-ON-POSITION
                   END-EVALUATE
               WHEN "reverse"
                   MOVE WS-PARAM2 TO WS-NUM1
                   MOVE WS-PARAM4 TO WS-NUM2
                   PERFORM REVERSE-POSITIONS
               WHEN "move"
                   MOVE WS-PARAM2 TO WS-NUM1
                   MOVE WS-PARAM5 TO WS-NUM2
                   PERFORM MOVE-POSITION
           END-EVALUATE.

       SWAP-POSITION SECTION.
           IF WS-NUM1 > WS-NUM2
               MOVE WS-NUM1 TO WS-TEMP-NUM
               MOVE WS-NUM2 TO WS-NUM1
               MOVE WS-TEMP-NUM TO WS-NUM2
           END-IF.
           MOVE WS-PASSWORD(WS-NUM1 + 1:1) TO WS-TEMP-CHAR.
           MOVE WS-PASSWORD(WS-NUM2 + 1:1) TO WS-PASSWORD(WS-NUM1 + 1:1).
           MOVE WS-TEMP-CHAR TO WS-PASSWORD(WS-NUM2 + 1:1).

       SWAP-LETTER SECTION.
           INSPECT WS-PASSWORD REPLACING ALL WS-CHAR1 BY LOW-VALUE.
           INSPECT WS-PASSWORD REPLACING ALL WS-CHAR2 BY WS-CHAR1.
           INSPECT WS-PASSWORD REPLACING ALL LOW-VALUE BY WS-CHAR2.

       ROTATE-LEFT SECTION.
           COMPUTE WS-STEPS = FUNCTION MOD(WS-STEPS, WS-PASSWORD-LEN).
           IF WS-STEPS = 0 THEN
               EXIT SECTION
           END-IF.
           MOVE WS-PASSWORD(WS-STEPS + 1: WS-PASSWORD-LEN - WS-STEPS)
               TO WS-TEMP-PASSWORD(1: WS-PASSWORD-LEN - WS-STEPS).
           MOVE WS-PASSWORD(1: WS-STEPS)
               TO WS-TEMP-PASSWORD(WS-PASSWORD-LEN - WS-STEPS + 1: WS-STEPS).
           MOVE WS-TEMP-PASSWORD TO WS-PASSWORD.

       ROTATE-RIGHT SECTION.
           COMPUTE WS-STEPS = FUNCTION MOD(WS-STEPS, WS-PASSWORD-LEN).
           IF WS-STEPS = 0 THEN
               EXIT SECTION
           END-IF.
           MOVE WS-PASSWORD(WS-PASSWORD-LEN - WS-STEPS + 1: WS-STEPS)
               TO WS-TEMP-PASSWORD(1: WS-STEPS).
           MOVE WS-PASSWORD(1: WS-PASSWORD-LEN - WS-STEPS)
               TO WS-TEMP-PASSWORD(WS-STEPS + 1: WS-PASSWORD-LEN - WS-STEPS).
           MOVE WS-TEMP-PASSWORD TO WS-PASSWORD.

       ROTATE-BASED-ON-POSITION SECTION.
           MOVE 0 TO WS-INDEX.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-PASSWORD-LEN
               IF WS-PASSWORD(WS-I:1) = WS-CHAR1
                   MOVE WS-I TO WS-INDEX
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           COMPUTE WS-STEPS = WS-INDEX.
           IF WS-INDEX >= 5 THEN
               ADD 1 TO WS-STEPS
           END-IF.
           PERFORM ROTATE-RIGHT.

       REVERSE-POSITIONS SECTION.
           IF WS-NUM1 > WS-NUM2
               MOVE WS-NUM1 TO WS-TEMP-NUM
               MOVE WS-NUM2 TO WS-NUM1
               MOVE WS-TEMP-NUM TO WS-NUM2
           END-IF.
           COMPUTE WS-SUBSTRING-LEN = WS-NUM2 - WS-NUM1 + 1.
           MOVE WS-PASSWORD(WS-NUM1 + 1:WS-SUBSTRING-LEN) TO WS-SUBSTRING.
           MOVE SPACES TO WS-REVERSED-SUBSTRING.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-SUBSTRING-LEN
               MOVE WS-SUBSTRING(WS-SUBSTRING-LEN - WS-I + 1:1)
                   TO WS-REVERSED-SUBSTRING(WS-I:1)
           END-PERFORM.
           STRING WS-PASSWORD(1:WS-NUM1)
                  WS-REVERSED-SUBSTRING(1:WS-SUBSTRING-LEN)
                  WS-PASSWORD(WS-NUM2 + 2:WS-PASSWORD-LEN - (WS-NUM2 + 1))
                  DELIMITED BY SIZE
                  INTO WS-TEMP-PASSWORD
           END-STRING.
           MOVE WS-TEMP-PASSWORD TO WS-PASSWORD.

       MOVE-POSITION SECTION.
           MOVE WS-PASSWORD(WS-NUM1 + 1:1) TO WS-TEMP-CHAR.

           STRING WS-PASSWORD(1:WS-NUM1)
                  WS-PASSWORD(WS-NUM1 + 2:WS-PASSWORD-LEN - WS-NUM1 - 1)
                  DELIMITED BY SIZE
                  INTO WS-PASSWORD-7-CHARS
           END-STRING.

           STRING WS-PASSWORD-7-CHARS(1:WS-NUM2)
                  WS-TEMP-CHAR
                  WS-PASSWORD-7-CHARS(WS-NUM2 + 1:WS-PASSWORD-LEN - WS-NUM2 - 1)
                  DELIMITED BY SIZE
                  INTO WS-PASSWORD
           END-STRING.
