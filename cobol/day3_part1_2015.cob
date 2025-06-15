
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SantaDelivery.
       AUTHOR. Expert Programmer.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  InputFile.
       01  DirectionsRecord PIC X(10000).

       WORKING-STORAGE SECTION.
       01  W-X             PIC S9(4) COMP VALUE 500.
       01  W-Y             PIC S9(4) COMP VALUE 500.
       01  W-I             PIC 9(5) COMP.
       01  W-Count         PIC 9(8) COMP VALUE 0.
       01  W-Directions    PIC X(10000).
       01  W-Directions-Len PIC 9(5) COMP.

       01  W-Grid.
           05 W-Row OCCURS 1000 TIMES.
              10 W-Col OCCURS 1000 TIMES.
                 15 W-Visited PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.
       main.
           OPEN INPUT InputFile.
           READ InputFile INTO W-Directions.
           CLOSE InputFile.

           MOVE FUNCTION LENGTH(FUNCTION TRIM(W-Directions))
             TO W-Directions-Len.

           MOVE 1 TO W-Visited(W-X, W-Y).
           ADD 1 TO W-Count.

           PERFORM VARYING W-I FROM 1 BY 1
               UNTIL W-I > W-Directions-Len

               EVALUATE W-Directions(W-I:1)
                   WHEN '^'
                       ADD 1 TO W-Y
                   WHEN 'v'
                       SUBTRACT 1 FROM W-Y
                   WHEN '>'
                       ADD 1 TO W-X
                   WHEN '<'
                       SUBTRACT 1 FROM W-X
               END-EVALUATE

               IF W-Visited(W-X, W-Y) = 0
                   MOVE 1 TO W-Visited(W-X, W-Y)
                   ADD 1 TO W-Count
               END-IF
           END-PERFORM.

           DISPLAY W-Count.
           STOP RUN.
