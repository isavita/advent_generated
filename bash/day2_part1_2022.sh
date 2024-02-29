
#!/bin/bash

totalScore=0

while IFS= read -r line || [ -n "$line" ]; do
    opponent="${line:0:1}"
    yourMove="${line:2:1}"

    score=0
    if [ "$yourMove" = "X" ]; then
        score=1
    elif [ "$yourMove" = "Y" ]; then
        score=2
    elif [ "$yourMove" = "Z" ]; then
        score=3
    fi

    if ([ "$opponent" = "A" ] && [ "$yourMove" = "Y" ]) || ([ "$opponent" = "B" ] && [ "$yourMove" = "Z" ]) || ([ "$opponent" = "C" ] && [ "$yourMove" = "X" ]); then
        score=$((score+6))
    elif ([ "$opponent" = "A" ] && [ "$yourMove" = "X" ]) || ([ "$opponent" = "B" ] && [ "$yourMove" = "Y" ]) || ([ "$opponent" = "C" ] && [ "$yourMove" = "Z" ]); then
        score=$((score+3))
    fi

    totalScore=$((totalScore+score))
done < "input.txt"

echo "$totalScore"
exit 0
