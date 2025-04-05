
#!/bin/bash

# Performs a spin operation on the program string
# $1: program string
# $2: number of positions to spin (X)
spin() {
    local progs="$1"
    local count="$2"
    local len=${#progs}
    # Ensure count is positive and within bounds
    count=$(( count % len ))
    if (( count < 0 )); then
        count=$(( count + len ))
    fi
    if (( count == 0 )); then
        echo "$progs"
    else
        echo "${progs: -count}${progs:0:len-count}"
    fi
}

# Performs an exchange operation on the program string by index
# $1: program string
# $2: index A
# $3: index B
exchange() {
    local progs="$1"
    local idxA="$2"
    local idxB="$3"
    local charA=${progs:idxA:1}
    local charB=${progs:idxB:1}

    # Build the new string piece by piece
    local result=""
    local len=${#progs}
    for (( i=0; i<len; i++ )); do
        if (( i == idxA )); then
            result+="$charB"
        elif (( i == idxB )); then
            result+="$charA"
        else
            result+="${progs:i:1}"
        fi
    done
    echo "$result"
}

# Performs a partner operation on the program string by value
# $1: program string
# $2: program name A
# $3: program name B
partner() {
    local progs="$1"
    local progA="$2"
    local progB="$3"

    # Use placeholder substitution for swapping characters
    local placeholder="__PLACEHOLDER__"
    progs=${progs//$progA/$placeholder}
    progs=${progs//$progB/$progA}
    progs=${progs//$placeholder/$progB}
    echo "$progs"
}


main() {
    local programs="abcdefghijklmnop"
    local moves_str
    moves_str=$(tr -d '\n' < input.txt) # Read and remove all newlines

    local IFS=',' # Set Internal Field Separator to comma
    local -a moves # Declare moves as an array
    read -ra moves <<< "$moves_str" # Read into array 'moves'

    local move
    for move in "${moves[@]}"; do
        local type=${move:0:1}
        local args=${move:1}

        case "$type" in
            s)
                programs=$(spin "$programs" "$args")
                ;;
            x)
                local idxA=${args%/*}
                local idxB=${args#*/}
                programs=$(exchange "$programs" "$idxA" "$idxB")
                ;;
            p)
                local progA=${args%/*}
                local progB=${args#*/}
                programs=$(partner "$programs" "$progA" "$progB")
                ;;
        esac
    done

    echo "$programs"
}

# Execute the main function
main
