
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FindLongestRoute.
       AUTHOR. System.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  InputFile.
       01  InputRecord         PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-File-Status      PIC X VALUE 'N'.
           88 Ws-EOF               VALUE 'Y'.

       01  WS-Input-Data.
           05 WS-From-Name     PIC X(20).
           05 WS-To-Name       PIC X(20).
           05 WS-Dist-Str      PIC X(5).

       01  WS-Location-Info.
           05 Location-Count   PIC 9(3) COMP VALUE 0.
           05 Temp-Name        PIC X(20).
           05 Location-Found   PIC X.

       01  WS-Indices.
           05 I                PIC 9(3) COMP.
           05 J                PIC 9(3) COMP.
           05 K                PIC 9(3) COMP.
           05 From-Idx         PIC 9(3) COMP.
           05 To-Idx           PIC 9(3) COMP.
           05 Temp-Idx         PIC 9(3) COMP.
           05 Current-Loc-Idx  PIC 9(3) COMP.

       01  WS-Calc-Fields.
           05 WS-Distance      PIC 9(5) COMP.
           05 Current-Distance PIC 9(9) COMP.
           05 Max-Distance     PIC 9(9) COMP VALUE 0.

       01  Locations-Table.
           05 Location-Data OCCURS 100 TIMES.
               10 Location-Name    PIC X(20).
               10 Distances OCCURS 100 TIMES PIC 9(5) COMP.

       01  Route-Table.
           05 Route-Entry OCCURS 100 TIMES PIC 9(3) COMP.

       01  Permutation-Control.
           05 C-Array OCCURS 100 TIMES PIC 9(3) COMP.

       PROCEDURE DIVISION.
       100-Main.
           INITIALIZE Locations-Table.
           PERFORM 200-Read-Input.
           PERFORM 300-Find-Longest-Route.
           DISPLAY Max-Distance.
           STOP RUN.

       200-Read-Input.
           OPEN INPUT InputFile.
           PERFORM UNTIL Ws-EOF
               READ InputFile INTO InputRecord
                   AT END SET Ws-EOF TO TRUE
                   NOT AT END PERFORM 210-Process-Line
               END-READ
           END-PERFORM.
           CLOSE InputFile.

       210-Process-Line.
           UNSTRING InputRecord DELIMITED BY " to " OR " = "
               INTO WS-From-Name, WS-To-Name, WS-Dist-Str.
           COMPUTE WS-Distance = FUNCTION NUMVAL(WS-Dist-Str).

           MOVE WS-From-Name TO Temp-Name.
           PERFORM 220-Get-Location-Index.
           MOVE Current-Loc-Idx TO From-Idx.

           MOVE WS-To-Name TO Temp-Name.
           PERFORM 220-Get-Location-Index.
           MOVE Current-Loc-Idx TO To-Idx.

           MOVE WS-Distance TO Distances(From-Idx, To-Idx).
           MOVE WS-Distance TO Distances(To-Idx, From-Idx).

       220-Get-Location-Index.
           MOVE 'N' TO Location-Found.
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > Location-Count OR Location-Found = 'Y'
               IF Location-Name(I) = Temp-Name
                   MOVE I TO Current-Loc-Idx
                   MOVE 'Y' TO Location-Found
               END-IF
           END-PERFORM.
           IF Location-Found = 'N'
               ADD 1 TO Location-Count
               MOVE Location-Count TO Current-Loc-Idx
               MOVE Temp-Name TO Location-Name(Current-Loc-Idx)
           END-IF.

       300-Find-Longest-Route.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > Location-Count
               MOVE I TO Route-Entry(I)
           END-PERFORM.
           INITIALIZE Permutation-Control.

           PERFORM 400-Calculate-Route-Distance.
           MOVE Current-Distance TO Max-Distance.

           MOVE 1 TO I.
           PERFORM UNTIL I >= Location-Count
               COMPUTE J = I + 1
               IF C-Array(J) < I
                   IF FUNCTION MOD(J, 2) = 1
                       MOVE Route-Entry(1) TO Temp-Idx
                       MOVE Route-Entry(J) TO Route-Entry(1)
                       MOVE Temp-Idx TO Route-Entry(J)
                   ELSE
                       COMPUTE K = C-Array(J) + 1
                       MOVE Route-Entry(K) TO Temp-Idx
                       MOVE Route-Entry(J) TO Route-Entry(K)
                       MOVE Temp-Idx TO Route-Entry(J)
                   END-IF
                   PERFORM 400-Calculate-Route-Distance
                   IF Current-Distance > Max-Distance
                       MOVE Current-Distance TO Max-Distance
                   END-IF
                   ADD 1 TO C-Array(J)
                   MOVE 1 TO I
               ELSE
                   MOVE 0 TO C-Array(J)
                   ADD 1 TO I
               END-IF
           END-PERFORM.

       400-Calculate-Route-Distance.
           MOVE 0 TO Current-Distance.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= Location-Count
               MOVE Route-Entry(I) TO From-Idx
               MOVE Route-Entry(I + 1) TO To-Idx
               ADD Distances(From-Idx, To-Idx) TO Current-Distance
           END-PERFORM.
