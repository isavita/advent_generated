#!/bin/bash

# Read favorite number from input.txt
favoriteNumber=$(cat input.txt)

# Checks if a given point is a wall
isWall() {
    local x=$1
    local y=$2
    local sum=$((x*x + 3*x + 2*x*y + y + y*y + favoriteNumber))
    local bits=$(echo "obase=2; $sum" | bc | tr -d '\n' | awk '{print gsub(/1/,"")}')
    return $((bits % 2))
}

# BFS to find shortest path
bfs() {
    local start="1,1"
    local target="31,39"
    local queue=("$start")
    local visited=("$start")
    local steps=0

    while [ ${#queue[@]} -gt 0 ]; do
        local size=${#queue[@]}
        for ((i=0; i<$size; i++)); do
            local point=(${queue[i]//,/ })
            local px=${point[0]}
            local py=${point[1]}

            if [[ "$px,$py" == "$target" ]]; then
                echo $steps
                return
            fi
            
            for dx in -1 0 1; do
                for dy in -1 0 1; do
                    if [ $((dx * dy)) -eq 0 ] && [ $((dx + dy)) -ne 0 ]; then
                        local nx=$((px + dx))
                        local ny=$((py + dy))
                        local next="$nx,$ny"
                        if [[ " ${visited[*]} " != *" $next "* ]] && [ $nx -ge 0 ] && [ $ny -ge 0 ]; then
                            isWall $nx $ny
                            if [ $? -eq 1 ]; then
                                continue
                            fi
                            queue+=("$next")
                            visited+=("$next")
                        fi
                    fi
                done
            done
        done
        queue=("${queue[@]:$size}")
        ((steps++))
    done

    echo -1
}

# Run BFS and print result
bfs