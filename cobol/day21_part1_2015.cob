
IDENTIFICATION DIVISION.
PROGRAM-ID. ADVENT-OF-CODE-2015-DAY21.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT BOSS-STATS-FILE ASSIGN TO "input.txt"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD BOSS-STATS-FILE.
01 BOSS-STAT-RECORD PIC X(80).

WORKING-STORAGE SECTION.
*> Boss Stats from input.txt
01 WS-BOSS-STATS.
   05 WS-BOSS-HP         PIC 9(3).
   05 WS-BOSS-DAMAGE     PIC 9(2).
   05 WS-BOSS-ARMOR      PIC 9(1).

*> Player Stats (fixed)
01 WS-PLAYER-STATS.
   05 WS-PLAYER-HP       PIC 9(3) VALUE 100.

*> Game Items Data
01 WS-WEAPONS-TABLE.
   05 WS-WEAPON-ENTRY OCCURS 5 TIMES.
      10 WS-W-NAME        PIC X(15).
      10 WS-W-COST        PIC 9(3).
      10 WS-W-DAMAGE      PIC 9(2).
      10 WS-W-ARMOR       PIC 9(1).

01 WS-WEAPONS-DATA REDEFINES WS-WEAPONS-TABLE.
   05 FILLER PIC X(21) VALUE "Dagger         008040".
   05 FILLER PIC X(21) VALUE "Shortsword     010050".
   05 FILLER PIC X(21) VALUE "Warhammer      025060".
   05 FILLER PIC X(21) VALUE "Longsword      040070".
   05 FILLER PIC X(21) VALUE "Greataxe       074080".

01 WS-ARMOR-TABLE.
   05 WS-ARMOR-ENTRY OCCURS 6 TIMES.
      10 WS-A-NAME        PIC X(15).
      10 WS-A-COST        PIC 9(3).
      10 WS-A-DAMAGE      PIC 9(1). *> Armor items have 0 damage
      10 WS-A-ARMOR       PIC 9(1).

01 WS-ARMOR-DATA REDEFINES WS-ARMOR-TABLE.
   05 FILLER PIC X(21) VALUE "None           000000".
   05 FILLER PIC X(21) VALUE "Leather        013001".
   05 FILLER PIC X(21) VALUE "Chainmail      031002".
   05 FILLER PIC X(21) VALUE "Splintmail     053003".
   05 FILLER PIC X(21) VALUE "Bandedmail     075004".
   05 FILLER PIC X(21) VALUE "Platemail      102005".

01 WS-RINGS-TABLE.
   05 WS-RING-ENTRY OCCURS 7 TIMES.
      10 WS-R-NAME        PIC X(15).
      10 WS-R-COST        PIC 9(3).
      10 WS-R-DAMAGE      PIC 9(1).
      10 WS-R-ARMOR       PIC 9(1).

01 WS-RINGS-DATA REDEFINES WS-RINGS-TABLE.
   05 FILLER PIC X(21) VALUE "None           000000".
   05 FILLER PIC X(21) VALUE "Damage +1      025010".
   05 FILLER PIC X(21) VALUE "Damage +2      050020".
   05 FILLER PIC X(21) VALUE "Damage +3      100030".
   05 FILLER PIC X(21) VALUE "Defense +1     020001".
   05 FILLER PIC X(21) VALUE "Defense +2     040002".
   05 FILLER PIC X(21) VALUE "Defense +3     080003".

*> Calculation Variables
01 WS-MIN-GOLD          PIC 9(5) VALUE 99999.

01 WS-CURRENT-COST      PIC 9(5).
01 WS-PLAYER-TOTAL-DAMAGE PIC 9(2).
01 WS-PLAYER-TOTAL-ARMOR  PIC 9(2).

01 WS-PLAYER-TURNS      PIC 9(3).
01 WS-BOSS-TURNS        PIC 9(3).

01 WS-EFFECTIVE-PLAYER-DAMAGE PIC 9(3).
01 WS-EFFECTIVE-BOSS-DAMAGE   PIC 9(3).

*> Loop Counters
01 WS-W-IDX             PIC 9(1).
01 WS-A-IDX             PIC 9(1).
01 WS-R1-IDX            PIC 9(1).
01 WS-R2-IDX            PIC 9(1).

PROCEDURE DIVISION.
MAIN-LOGIC.
    PERFORM READ-BOSS-STATS.
    PERFORM CALCULATE-MIN-GOLD.
    DISPLAY WS-MIN-GOLD.
    STOP RUN.

READ-BOSS-STATS.
    OPEN INPUT BOSS-STATS-FILE.
    READ BOSS-STATS-FILE INTO BOSS-STAT-RECORD
        AT END DISPLAY "Error: Could not read Boss HP"
        STOP RUN.
    MOVE BOSS-STAT-RECORD TO WS-BOSS-HP.

    READ BOSS-STATS-FILE INTO BOSS-STAT-RECORD
        AT END DISPLAY "Error: Could not read Boss Damage"
        STOP RUN.
    MOVE BOSS-STAT-RECORD TO WS-BOSS-DAMAGE.

    READ BOSS-STATS-FILE INTO BOSS-STAT-RECORD
        AT END DISPLAY "Error: Could not read Boss Armor"
        STOP RUN.
    MOVE BOSS-STAT-RECORD TO WS-BOSS-ARMOR.
    CLOSE BOSS-STATS-FILE.

CALCULATE-MIN-GOLD.
    PERFORM VARYING WS-W-IDX FROM 1 BY 1 UNTIL WS-W-IDX > 5
        PERFORM VARYING WS-A-IDX FROM 1 BY 1 UNTIL WS-A-IDX > 6
            PERFORM VARYING WS-R1-IDX FROM 1 BY 1 UNTIL WS-R1-IDX > 7
                PERFORM VARYING WS-R2-IDX FROM WS-R1-IDX + 1 BY 1 UNTIL WS-R2-IDX > 7
                    PERFORM PROCESS-COMBINATION
                END-PERFORM
            END-PERFORM
        END-PERFORM
    END-PERFORM.

PROCESS-COMBINATION.
    COMPUTE WS-CURRENT-COST = WS-W-COST(WS-W-IDX)
                            + WS-A-COST(WS-A-IDX)
                            + WS-R-COST(WS-R1-IDX)
                            + WS-R-COST(WS-R2-IDX).

    COMPUTE WS-PLAYER-TOTAL-DAMAGE = WS-W-DAMAGE(WS-W-IDX)
                                   + WS-A-DAMAGE(WS-A-IDX)
                                   + WS-R-DAMAGE(WS-R1-IDX)
                                   + WS-R-DAMAGE(WS-R2-IDX).

    COMPUTE WS-PLAYER-TOTAL-ARMOR = WS-W-ARMOR(WS-W-IDX)
                                  + WS-A-ARMOR(WS-A-IDX)
                                  + WS-R-ARMOR(WS-R1-IDX)
                                  + WS-R-ARMOR(WS-R2-IDX).

    COMPUTE WS-EFFECTIVE-PLAYER-DAMAGE = WS-PLAYER-TOTAL-DAMAGE - WS-BOSS-ARMOR.
    IF WS-EFFECTIVE-PLAYER-DAMAGE < 1 THEN
        MOVE 1 TO WS-EFFECTIVE-PLAYER-DAMAGE
    END-IF.
    COMPUTE WS-PLAYER-TURNS = (WS-BOSS-HP + WS-EFFECTIVE-PLAYER-DAMAGE - 1)
                            / WS-EFFECTIVE-PLAYER-DAMAGE.

    COMPUTE WS-EFFECTIVE-BOSS-DAMAGE = WS-BOSS-DAMAGE - WS-PLAYER-TOTAL-ARMOR.
    IF WS-EFFECTIVE-BOSS-DAMAGE < 1 THEN
        MOVE 1 TO WS-EFFECTIVE-BOSS-DAMAGE
    END-IF.
    COMPUTE WS-BOSS-TURNS = (WS-PLAYER-HP + WS-EFFECTIVE-BOSS-DAMAGE - 1)
                          / WS-EFFECTIVE-BOSS-DAMAGE.

    IF WS-PLAYER-TURNS <= WS-BOSS-TURNS THEN
        IF WS-CURRENT-COST < WS-MIN-GOLD THEN
            MOVE WS-CURRENT-COST TO WS-MIN-GOLD
        END-IF
    END-IF.
