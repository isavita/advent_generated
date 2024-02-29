
#!/bin/bash

# Step 1: Read Input
IFS=$'\n' read -d '' -r -a lines < input.txt

# Initialize Variables
index=0
steps=0

# Navigate Maze
while [ $index -ge 0 ] && [ $index -lt ${#lines[@]} ]; do
    # Fetch the jump offset at the current index
    jump=${lines[$index]}

    # Update Offset
    lines[$index]=$((${lines[$index]} + 1))

    # Move to the new index
    index=$(($index + $jump))

    # Increment steps counter
    steps=$(($steps + 1))
done

# Output
echo $steps
