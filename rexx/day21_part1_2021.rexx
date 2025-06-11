
/* REXX */
call main
exit

main:
    parse value linein('input.txt') with . ':' player1Pos
    parse value linein('input.txt') with . ':' player2Pos
    player1Pos = strip(player1Pos)
    player2Pos = strip(player2Pos)

    player1Score = 0
    player2Score = 0
    dieRoll = 1
    rollCount = 0

    do forever
        /* Player 1 */
        rolls = (dieRoll // 100) + ((dieRoll + 1) // 100) + ((dieRoll + 2) // 100)
        rollCount = rollCount + 3
        dieRoll = dieRoll + 3
        player1Pos = (player1Pos + rolls - 1) // 10 + 1
        player1Score = player1Score + player1Pos
        if player1Score >= 1000 then do
            say player2Score * rollCount
            leave
        end

        /* Player 2 */
        rolls = (dieRoll // 100) + ((dieRoll + 1) // 100) + ((dieRoll + 2) // 100)
        rollCount = rollCount + 3
        dieRoll = dieRoll + 3
        player2Pos = (player2Pos + rolls - 1) // 10 + 1
        player2Score = player2Score + player2Pos
        if player2Score >= 1000 then do
            say player1Score * rollCount
            leave
        end
    end
return
