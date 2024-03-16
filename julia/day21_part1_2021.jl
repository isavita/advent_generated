open("input.txt", "r") do file
    lines = readlines(file)
    player1Start = parse(Int, strip(lines[1][29:end]))
    player2Start = parse(Int, strip(lines[2][29:end]))
    player1Pos = player1Start
    player2Pos = player2Start

    player1Score = 0
    player2Score = 0

    dieRoll = 1
    rollCount = 0

    while true
        # Player 1
        rolls = (dieRoll%100) + ((dieRoll+1)%100) + ((dieRoll+2)%100)
        rollCount += 3
        dieRoll += 3

        player1Pos = ((player1Pos+rolls-1)%10) + 1
        player1Score += player1Pos

        if player1Score >= 1000
            println("Result:", player2Score*rollCount)
            break
        end

        # Player 2
        rolls = (dieRoll%100) + ((dieRoll+1)%100) + ((dieRoll+2)%100)
        rollCount += 3
        dieRoll += 3

        player2Pos = ((player2Pos+rolls-1)%10) + 1
        player2Score += player2Pos

        if player2Score >= 1000
            println(player1Score * rollCount)
            break
        end
    end
end