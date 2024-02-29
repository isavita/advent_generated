
#!/bin/bash

totalScore=0

while IFS= read -r line || [ -n "$line" ]; do
	opponent="${line:0:1}"
	roundEnd="${line:2:1}"

	if [ "$roundEnd" == "X" ]; then
		if [ "$opponent" == "A" ]; then
			yourMove="Z"
		elif [ "$opponent" == "B" ]; then
			yourMove="X"
		else
			yourMove="Y"
		fi
	elif [ "$roundEnd" == "Y" ]; then
		if [ "$opponent" == "A" ]; then
			yourMove="X"
		elif [ "$opponent" == "B" ]; then
			yourMove="Y"
		else
			yourMove="Z"
		fi
	else
		if [ "$opponent" == "A" ]; then
			yourMove="Y"
		elif [ "$opponent" == "B" ]; then
			yourMove="Z"
		else
			yourMove="X"
		fi
	fi

	score=0
	if [ "$yourMove" == "X" ]; then
		score=1
	elif [ "$yourMove" == "Y" ]; then
		score=2
	elif [ "$yourMove" == "Z" ]; then
		score=3
	fi

	if { [ "$opponent" == "A" ] && [ "$yourMove" == "Y" ]; } || { [ "$opponent" == "B" ] && [ "$yourMove" == "Z" ]; } || { [ "$opponent" == "C" ] && [ "$yourMove" == "X" ]; }; then
		score=$((score + 6))
	elif { [ "$opponent" == "A" ] && [ "$yourMove" == "X" ]; } || { [ "$opponent" == "B" ] && [ "$yourMove" == "Y" ]; } || { [ "$opponent" == "C" ] && [ "$yourMove" == "Z" ]; }; then
		score=$((score + 3))
	fi

	totalScore=$((totalScore + score))
done < "input.txt"

echo "$totalScore"
exit 0
