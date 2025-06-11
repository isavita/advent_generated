
/* REXX */
main:
    line = STRIP(LINEIN('input.txt'))

    spoken. = 0
    turn = 1

    DO WHILE POS(',', line) > 0
        PARSE VAR line number ',' line
        spoken.number = turn
        turn = turn + 1
    END
    lastSpoken = line

    DO t = turn + 1 TO 30000000
        lastTurn = spoken.lastSpoken
        spoken.lastSpoken = t - 1
        IF lastTurn = 0 THEN
            lastSpoken = 0
        ELSE
            lastSpoken = (t - 1) - lastTurn
    END

    SAY lastSpoken
EXIT
