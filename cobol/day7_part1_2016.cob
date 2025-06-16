
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLS-CHECKER.
       AUTHOR. Your Name.
       DATE-WRITTEN. 2023-11-27.
       REMARKS.
           Advent of Code 2016, Day 7: Internet Protocol Version 7.
           Counts IP addresses that support TLS.
           An IP supports TLS if it has an ABBA (xyyx, x!=y)
           in a supernet sequence (outside brackets) and no ABBA
           in any hypernet sequence (inside brackets).

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'input.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO 'stdout'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD           PIC X(250). *> Max expected IP length

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD          PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS         PIC X(02) VALUE '00'.
           88  END-OF-INPUT-FILE            VALUE '10'.
       01  WS-CURRENT-IP-ADDRESS  PIC X(250).
       01  WS-IP-LENGTH           PIC 9(03). *> Actual length of current IP

       01  WS-TLS-SUPPORTED-COUNT PIC 9(05) VALUE ZEROS.

       01  WS-IDX                 PIC 9(03).
       01  WS-C1                  PIC X(01).
       01  WS-C2                  PIC X(01).
       01  WS-C3                  PIC X(01).
       01  WS-C4                  PIC X(01).

       01  WS-FOUND-ABBA-SUPERNET PIC X(01) VALUE 'N'.
           88  ABBA-IN-SUPERNET             VALUE 'Y'.
       01  WS-FOUND-ABBA-HYPERNET PIC X(01) VALUE 'N'.
           88  ABBA-IN-HYPERNET             VALUE 'Y'.
       01  WS-IN-HYPERNET-SEQUENCE PIC X(01) VALUE 'N'.
           88  IN-HYPERNET                  VALUE 'Y'.

       01  WS-OUTPUT-MESSAGE      PIC X(80).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-PROGRAM.
           PERFORM PROCESS-INPUT-FILE.
           PERFORM TERMINATE-PROGRAM.
           STOP RUN.

       INITIALIZE-PROGRAM.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
           IF WS-FILE-STATUS NOT = '00'
               DISPLAY 'Error opening input.txt: ' WS-FILE-STATUS
               STOP RUN
           END-IF.
           PERFORM READ-NEXT-RECORD.

       PROCESS-INPUT-FILE.
           PERFORM UNTIL END-OF-INPUT-FILE
               PERFORM CHECK-TLS-SUPPORT
               PERFORM READ-NEXT-RECORD
           END-PERFORM.

       READ-NEXT-RECORD.
           READ INPUT-FILE INTO WS-CURRENT-IP-ADDRESS
               VARYING WS-IP-LENGTH
               AT END
                   SET END-OF-INPUT-FILE TO TRUE
               NOT AT END
                   CONTINUE
           END-READ.

       CHECK-TLS-SUPPORT.
           MOVE 'N' TO WS-FOUND-ABBA-SUPERNET.
           MOVE 'N' TO WS-FOUND-ABBA-HYPERNET.
           MOVE 'N' TO WS-IN-HYPERNET-SEQUENCE.

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-IP-LENGTH
               EVALUATE WS-CURRENT-IP-ADDRESS (WS-IDX : 1)
                   WHEN '['
                       SET IN-HYPERNET TO TRUE
                   WHEN ']'
                       SET IN-HYPERNET TO FALSE
                   WHEN OTHER
                       IF WS-IDX + 3 <= WS-IP-LENGTH
                           PERFORM CHECK-ABBA-WINDOW
                       END-IF
               END-EVALUATE
           END-PERFORM.

           IF ABBA-IN-SUPERNET AND NOT ABBA-IN-HYPERNET
               ADD 1 TO WS-TLS-SUPPORTED-COUNT
           END-IF.

       CHECK-ABBA-WINDOW.
           MOVE WS-CURRENT-IP-ADDRESS (WS-IDX : 1)     TO WS-C1.
           MOVE WS-CURRENT-IP-ADDRESS (WS-IDX + 1 : 1) TO WS-C2.
           MOVE WS-CURRENT-IP-ADDRESS (WS-IDX + 2 : 1) TO WS-C3.
           MOVE WS-CURRENT-IP-ADDRESS (WS-IDX + 3 : 1) TO WS-C4.

           IF WS-C1 = WS-C4 AND WS-C2 = WS-C3 AND WS-C1 NOT = WS-C2
               IF IN-HYPERNET
                   SET ABBA-IN-HYPERNET TO TRUE
               ELSE
                   SET ABBA-IN-SUPERNET TO TRUE
               END-IF
           END-IF.

       TERMINATE-PROGRAM.
           CLOSE INPUT-FILE.
           STRING "Total IPs supporting TLS: "
                  WS-TLS-SUPPORTED-COUNT DELIMITED BY SIZE
                  INTO WS-OUTPUT-MESSAGE
           END-STRING.
           WRITE OUTPUT-RECORD FROM WS-OUTPUT-MESSAGE.
           CLOSE OUTPUT-FILE.
