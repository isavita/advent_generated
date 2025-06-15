
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ReindeerRace.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  InputFile.
       01  InputRecord         PIC X(100).
       
       WORKING-STORAGE SECTION.
       01  WSEofFlag           PIC A(1) VALUE 'N'.
           88 WSEof           VALUE 'Y'.
       
       01  WSTotalTime         PIC 9(4) VALUE 2503.
       01  WSMaxDistance       PIC 9(8) VALUE 0.
       01  WSCurrentDistance   PIC 9(8).
       
       01  WSCycleTime         PIC 9(5).
       01  WSNumCycles         PIC 9(5).
       01  WSRemainderTime     PIC 9(5).
       01  WSFlyDistance       PIC 9(8).
       01  WSRemainderDist     PIC 9(8).
       
       01  WSReindeerTable.
           05 WSReindeer OCCURS 10 TIMES INDEXED BY RIndex.
              10 WSSpeed       PIC 9(4).
              10 WSFlyTime     PIC 9(4).
              10 WSRestTime    PIC 9(4).
       
       01  WSParseFields.
           05 PName            PIC X(20).
           05 PSpeed           PIC X(5).
           05 PFlyTime         PIC X(5).
           05 PRestTime        PIC X(5).
       
       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT InputFile.
           SET RIndex TO 1.
           PERFORM UNTIL WSEof
               READ InputFile
                   AT END SET WSEof TO TRUE
                   NOT AT END
                       UNSTRING InputRecord DELIMITED BY " can fly "
                           OR " km/s for "
                           OR " seconds, but then must rest for "
                           OR " seconds."
                           INTO PName, PSpeed, PFlyTime, PRestTime
                       END-UNSTRING
                       COMPUTE WSSpeed(RIndex) = FUNCTION NUMVAL(PSpeed)
                       COMPUTE WSFlyTime(RIndex) = 
                           FUNCTION NUMVAL(PFlyTime)
                       COMPUTE WSRestTime(RIndex) = 
                           FUNCTION NUMVAL(PRestTime)
                       SET RIndex UP BY 1
               END-READ
           END-PERFORM.
           CLOSE InputFile.
       
           PERFORM VARYING RIndex FROM 1 BY 1 UNTIL RIndex > 10
               COMPUTE WSCycleTime = 
                   WSFlyTime(RIndex) + WSRestTime(RIndex)
       
               DIVIDE WSTotalTime BY WSCycleTime
                   GIVING WSNumCycles REMAINDER WSRemainderTime
       
               COMPUTE WSFlyDistance = 
                   WSNumCycles * WSFlyTime(RIndex) * WSSpeed(RIndex)
       
               IF WSRemainderTime > WSFlyTime(RIndex)
                   COMPUTE WSRemainderDist = 
                       WSFlyTime(RIndex) * WSSpeed(RIndex)
               ELSE
                   COMPUTE WSRemainderDist = 
                       WSRemainderTime * WSSpeed(RIndex)
               END-IF
       
               COMPUTE WSCurrentDistance = 
                   WSFlyDistance + WSRemainderDist
       
               IF WSCurrentDistance > WSMaxDistance
                   MOVE WSCurrentDistance TO WSMaxDistance
               END-IF
           END-PERFORM.
       
           DISPLAY WSMaxDistance.
           STOP RUN.
