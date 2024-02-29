
#!/bin/bash

input=$(cat input.txt)
IFS="," read -ra directions <<< "$input"

x=0
y=0
z=0
maxDistance=0

abs() {
    if [ $1 -lt 0 ]; then
        echo $((-$1))
    else
        echo $1
    fi
}

max() {
    if [ $1 -gt $2 ]; then
        echo $1
    else
        echo $2
    fi
}

distance() {
    echo $(( ($(abs $1) + $(abs $2) + $(abs $3)) / 2 ))
}

for dir in "${directions[@]}"; do
    case $dir in
        "n")
            ((y++))
            ((z--))
            ;;
        "ne")
            ((x++))
            ((z--))
            ;;
        "se")
            ((x++))
            ((y--))
            ;;
        "s")
            ((y--))
            ((z++))
            ;;
        "sw")
            ((x--))
            ((z++))
            ;;
        "nw")
            ((x--))
            ((y++))
            ;;
    esac

    curDistance=$(distance $x $y $z)
    maxDistance=$(max $maxDistance $curDistance)
done

echo $(distance $x $y $z)
