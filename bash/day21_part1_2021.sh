
#!/bin/bash

main() {
    # Read starting positions from input.txt
    { read -r line1; read -r line2; } < "input.txt"
    player1Start=${line1:28}
    player2Start=${line2:28}

    # Initialize game state
    player1Pos=$((player1Start))
    player2Pos=$((player2Start))
    player1Score=0
    player2Score=0
    dieRoll=1
    rollCount=0

    while true; do
        # Player 1's turn
        rolls=$(( (dieRoll % 100) + ((dieRoll + 1) % 100) + ((dieRoll + 2) % 100) ))
        (( rollCount += 3 ))
        (( dieRoll += 3 ))

        (( player1Pos = (player1Pos + rolls - 1) % 10 + 1 ))
        (( player1Score += player1Pos ))

        if (( player1Score >= 1000 )); then
            echo $(( player2Score * rollCount ))
            break
        fi

        # Player 2's turn
        rolls=$(( (dieRoll % 100) + ((dieRoll + 1) % 100) + ((dieRoll + 2) % 100) ))
        (( rollCount += 3 ))
        (( dieRoll += 3 ))

        (( player2Pos = (player2Pos + rolls - 1) % 10 + 1 ))
        (( player2Score += player2Pos ))

        if (( player2Score >= 1000 )); then
            echo $(( player1Score * rollCount ))
            break
        fi
    done
}

main
