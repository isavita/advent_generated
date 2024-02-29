
#!/bin/bash

file="input.txt"
grid=()
x=0
y=0

while IFS= read -r line; do
    grid+=("$line")
done < "$file"

for ((i=0; i<${#grid[0]}; i++)); do
    if [ "${grid[0]:i:1}" = "|" ]; then
        x=$i
        break
    fi
done

dx=0
dy=1
steps=0

while true; do
    if [ $x -lt 0 ] || [ $x -ge ${#grid[0]} ] || [ $y -lt 0 ] || [ $y -ge ${#grid[@]} ]; then
        break
    fi

    cell="${grid[$y]:$x:1}"

    if [ "$cell" = " " ]; then
        break
    fi

    ((steps++))

    if [ "$cell" = "+" ]; then
        if [ $dx -eq 0 ]; then
            if [ $x -gt 0 ] && { [ "${grid[$y]:x-1:1}" = "-" ] || [ "${grid[$y]:x-1:1}" = [A-Z] ]; }; then
                dx=-1
                dy=0
            else
                dx=1
                dy=0
            fi
        else
            if [ $y -gt 0 ] && { [ "${grid[y-1]:x:1}" = "|" ] || [ "${grid[y-1]:x:1}" = [A-Z] ]; }; then
                dx=0
                dy=-1
            else
                dx=0
                dy=1
            fi
        fi
    fi

    ((x+=dx))
    ((y+=dy))
done

echo $steps
exit 0
