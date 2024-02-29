#!/bin/bash

player1=()
player2=()

current_player=1

if [ ! -f "input.txt" ]; then
  echo "Error: Input file 'input.txt' not found or cannot be read."
  exit 1
fi

while IFS= read -r line || [ -n "$line" ]; do
    if [[ $line == "Player 1:" ]]; then
        current_player=1
    elif [[ $line == "Player 2:" ]]; then
        current_player=2
    elif [[ ! -z $line ]]; then
        if [[ $current_player -eq 1 ]]; then
            player1+=($line)
        elif [[ $current_player -eq 2 ]]; then
            player2+=($line)
        fi
    fi
done < input.txt

while [[ ${#player1[@]} -gt 0 && ${#player2[@]} -gt 0 ]]; do
    p1=${player1[0]}
    p2=${player2[0]}

    player1=("${player1[@]:1}")
    player2=("${player2[@]:1}")

    if [[ $p1 -gt $p2 ]]; then
        player1+=($p1 $p2)
    else
        player2+=($p2 $p1)
    fi
done

winner=()
if [[ ${#player1[@]} -gt 0 ]]; then
    winner=("${player1[@]}")
else
    winner=("${player2[@]}")
fi

score=0
multiplier=${#winner[@]}
for card in "${winner[@]}"; do
    score=$((score + card * multiplier))
    multiplier=$((multiplier - 1))
done

echo $score