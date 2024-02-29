
#!/bin/bash

data=$(<input.txt)
imageData=$(echo $data | tr -d '\n')

width=25
height=6
layerSize=$((width * height))

minZeros=$((layerSize + 1))
result=0

for ((i = 0; i < ${#imageData}; i += layerSize)); do
	layer=${imageData:i:layerSize}
	zeroCount=0
	oneCount=0
	twoCount=0

	for ((j = 0; j < ${#layer}; j++)); do
		pixel=${layer:j:1}
		case $pixel in
			"0")
				((zeroCount++))
				;;
			"1")
				((oneCount++))
				;;
			"2")
				((twoCount++))
				;;
		esac
	done

	if ((zeroCount < minZeros)); then
		minZeros=$zeroCount
		result=$((oneCount * twoCount))
	fi
done

echo $result
exit 0
