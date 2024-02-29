
#!/bin/bash

firstRow=$(head -n 1 input.txt)
totalRows=40

countSafeTiles() {
    local currentRow=$1
    local safeCount=$(countChar "$currentRow" '.')

    for ((i = 1; i < totalRows; i++)); do
        local nextRow=""
        for ((j = 0; j < ${#currentRow}; j++)); do
            if [[ $(isTrap $((j-1)) $j $((j+1)) "$currentRow") == true ]]; then
                nextRow+="^"
            else
                nextRow+="."
                ((safeCount++))
            fi
        done
        currentRow=$nextRow
    done
    echo $safeCount
}

isTrap() {
    local left=$1
    local center=$2
    local right=$3
    local row=$4

    local l=$(safeIfOutOfBounds $left "$row")
    local c=${row:$center:1}
    local r=$(safeIfOutOfBounds $right "$row")

    if [[ ($l == '^' && $c == '^' && $r == '.') || ($c == '^' && $r == '^' && $l == '.') || ($l == '^' && $c == '.' && $r == '.') || ($r == '^' && $c == '.' && $l == '.') ]]; then
        echo true
    else
        echo false
    fi
}

safeIfOutOfBounds() {
    local index=$1
    local row=$2

    if [[ $index -lt 0 || $index -ge ${#row} ]]; then
        echo "."
    else
        echo "${row:$index:1}"
    fi
}

countChar() {
    local str=$1
    local char=$2
    local count=0

    for ((i = 0; i < ${#str}; i++)); do
        if [[ ${str:$i:1} == $char ]]; then
            ((count++))
        fi
    done
    echo $count
}

safeTilesCount=$(countSafeTiles "$firstRow")
echo $safeTilesCount
exit 0
