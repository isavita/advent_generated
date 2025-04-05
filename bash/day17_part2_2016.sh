
#!/bin/bash

longest=0
passcode=""

is_open() {
    local char=$1
    [[ $char == [b-f] ]]
}

find_path() {
    local x=$1
    local y=$2
    local path=$3
    local path_len=${#path}

    if [[ $x -eq 3 && $y -eq 3 ]]; then
        if [[ $path_len -gt $longest ]]; then
            longest=$path_len
        fi
        return
    fi

    local hash
    hash=$(printf "%s" "${passcode}${path}" | md5sum)
    hash=${hash%% *} # Keep only the hash part

    # Up
    if is_open "${hash:0:1}"; then
        local next_y=$((y - 1))
        if [[ $next_y -ge 0 ]]; then
            find_path "$x" "$next_y" "${path}U"
        fi
    fi
    # Down
    if is_open "${hash:1:1}"; then
        local next_y=$((y + 1))
        if [[ $next_y -lt 4 ]]; then
            find_path "$x" "$next_y" "${path}D"
        fi
    fi
    # Left
    if is_open "${hash:2:1}"; then
        local next_x=$((x - 1))
        if [[ $next_x -ge 0 ]]; then
            find_path "$next_x" "$y" "${path}L"
        fi
    fi
    # Right
    if is_open "${hash:3:1}"; then
        local next_x=$((x + 1))
        if [[ $next_x -lt 4 ]]; then
            find_path "$next_x" "$y" "${path}R"
        fi
    fi
}

main() {
    if [[ ! -f "input.txt" ]]; then
         echo "Error: input.txt not found" >&2
         exit 1
    fi
    read -r passcode < "input.txt"
    if [[ -z "$passcode" ]]; then
         echo "Error: input.txt is empty or invalid" >&2
         exit 1
    fi

    find_path 0 0 ""
    echo "$longest"
}

main
