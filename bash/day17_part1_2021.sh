
#!/bin/bash

main() {
    read line < input.txt
    if [[ "$line" =~ target\ area:\ x=([0-9-]+)\.\.([0-9-]+),\ y=([0-9-]+)\.\.([0-9-]+) ]]; then
        local y_min=${BASH_REMATCH[3]}
    else
        echo "Error parsing input line" >&2
        exit 1
    fi

    local optimal_y_vel=$(( -y_min - 1 ))
    local max_y=$(( optimal_y_vel * (optimal_y_vel + 1) / 2 ))

    echo "$max_y"
}

main
