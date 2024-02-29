
#!/bin/bash

input=$(<input.txt)
sum=0

for ((i=0; i<${#input}; i++)); do
	next=$(( (i + 1) % ${#input} ))
	if [ "${input:i:1}" == "${input:next:1}" ]; then
		sum=$((sum + ${input:i:1}))
	fi
done

echo $sum
exit 0
