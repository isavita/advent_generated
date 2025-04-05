
#!/bin/bash

main() {
    declare -i x=0
    declare -i y=0
    declare -i waypoint_x=10
    declare -i waypoint_y=1
    declare -i value
    declare -i turns
    declare -i temp_x
    declare -i abs_x
    declare -i abs_y
    declare -i result
    local action

    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ -z "$line" ]]; then continue; fi

        action="${line:0:1}"
        value="${line:1}"

        case "$action" in
            N) ((waypoint_y += value)) ;;
            S) ((waypoint_y -= value)) ;;
            E) ((waypoint_x += value)) ;;
            W) ((waypoint_x -= value)) ;;
            L)
                ((turns = value / 90))
                for ((i = 0; i < turns; i++)); do
                    temp_x=$waypoint_x
                    ((waypoint_x = -waypoint_y))
                    ((waypoint_y = temp_x))
                done
                ;;
            R)
                ((turns = value / 90))
                for ((i = 0; i < turns; i++)); do
                    temp_x=$waypoint_x
                    ((waypoint_x = waypoint_y))
                    ((waypoint_y = -temp_x))
                done
                ;;
            F)
                ((x += waypoint_x * value))
                ((y += waypoint_y * value))
                ;;
        esac
    done < "input.txt"

    (( abs_x = x < 0 ? -x : x ))
    (( abs_y = y < 0 ? -y : y ))
    (( result = abs_x + abs_y ))

    echo "$result"
}

main
