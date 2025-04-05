
#!/bin/bash

# --- Helper Functions ---

swap_position() {
    local password=$1
    local x=$2
    local y=$3
    local len=${#password}

    if [[ $x -gt $y ]]; then
        local tmp=$x
        x=$y
        y=$tmp
    fi

    local char_x=${password:x:1}
    local char_y=${password:y:1}
    local prefix=${password:0:x}
    local middle=${password:x+1:y-x-1}
    local suffix=${password:y+1}

    echo "${prefix}${char_y}${middle}${char_x}${suffix}"
}

swap_letter() {
    local password=$1
    local x=$2
    local y=$3
    # Use a temporary character unlikely to be in the password
    local temp="@"
    password=$(echo "$password" | sed "s/$x/$temp/g")
    password=$(echo "$password" | sed "s/$y/$x/g")
    password=$(echo "$password" | sed "s/$temp/$y/g")
    echo "$password"
}

rotate_left() {
    local password=$1
    local steps=$2
    local len=${#password}
    steps=$(( steps % len ))
    if [[ $steps -eq 0 ]]; then
      echo "$password"
      return
    fi
    echo "${password:steps}${password:0:steps}"
}

rotate_right() {
    local password=$1
    local steps=$2
    local len=${#password}
    steps=$(( steps % len ))
     if [[ $steps -eq 0 ]]; then
      echo "$password"
      return
    fi
    local split_at=$(( len - steps ))
    echo "${password:split_at}${password:0:split_at}"
}

rotate_based_on_position() {
    local password=$1
    local x=$2
    local len=${#password}
    local index=-1

    # Find index (bash string manipulation is 0-indexed)
    local temp_pass="$password"
    local i=0
    while [[ $i -lt $len ]]; do
        if [[ "${temp_pass:i:1}" == "$x" ]]; then
            index=$i
            break
        fi
        i=$((i + 1))
    done


    if [[ $index -eq -1 ]]; then
        echo "$password" # Should not happen based on problem description
        return
    fi

    local steps=$(( 1 + index ))
    if [[ $index -ge 4 ]]; then
        steps=$(( steps + 1 ))
    fi

    rotate_right "$password" "$steps"
}

reverse_positions() {
    local password=$1
    local x=$2
    local y=$3
    local len=${#password}

     if [[ $x -gt $y ]]; then
        local tmp=$x
        x=$y
        y=$tmp
    fi

    local prefix=${password:0:x}
    local middle=${password:x:y-x+1}
    local suffix=${password:y+1}

    local reversed_middle=$(echo "$middle" | rev)

    echo "${prefix}${reversed_middle}${suffix}"
}

move_position() {
    local password=$1
    local x=$2
    local y=$3
    local len=${#password}

    local char_x=${password:x:1}
    # String without char_x
    local temp_pass="${password:0:x}${password:x+1}"

    # Insert char_x at position y
    echo "${temp_pass:0:y}${char_x}${temp_pass:y}"
}


# --- Main Logic ---

main() {
    local password="abcdefgh"
    local input_file="input.txt"

    if [[ ! -f "$input_file" ]]; then
       echo "Error: Input file '$input_file' not found." >&2
       exit 1
    fi

    while IFS= read -r line || [[ -n "$line" ]]; do
        local fields=($line)
        local command="${fields[0]}"
        local type="${fields[1]}"

        case "$command" in
            swap)
                if [[ "$type" == "position" ]]; then
                    password=$(swap_position "$password" "${fields[2]}" "${fields[5]}")
                elif [[ "$type" == "letter" ]]; then
                    password=$(swap_letter "$password" "${fields[2]}" "${fields[5]}")
                fi
                ;;
            rotate)
                if [[ "$type" == "left" ]]; then
                    password=$(rotate_left "$password" "${fields[2]}")
                elif [[ "$type" == "right" ]]; then
                     password=$(rotate_right "$password" "${fields[2]}")
                elif [[ "$type" == "based" ]]; then
                     password=$(rotate_based_on_position "$password" "${fields[6]}")
                fi
                ;;
            reverse)
                 password=$(reverse_positions "$password" "${fields[2]}" "${fields[4]}")
                ;;
            move)
                 password=$(move_position "$password" "${fields[2]}" "${fields[5]}")
                ;;
        esac
    done < "$input_file"

    echo "$password"
}

main

