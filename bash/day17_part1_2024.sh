
#!/bin/bash

# Initialize registers and program array
A=0
B=0
C=0
declare -a program
declare -a output_vals

# Function to get combo value based on operand
get_combo_val() {
    local op=$1
    if (( op <= 3 )); then
        echo "$op"
    elif (( op == 4 )); then
        echo "$A"
    elif (( op == 5 )); then
        echo "$B"
    elif (( op == 6 )); then
        echo "$C"
    else
        # Invalid operand, echo something predictable or handle error
        # Based on Python, this case shouldn't be hit with valid input
        echo "0" # Defaulting to 0 if invalid
    fi
}

# Main execution logic
main() {
    # Read input file and parse values
    while IFS= read -r line || [[ -n "$line" ]]; do
        shopt -s extglob # Enable extended pattern matching
        line="${line##*( )}" # Trim leading spaces
        line="${line%%*( )}" # Trim trailing spaces
        shopt -u extglob # Disable extended pattern matching

        if [[ -z "$line" ]]; then
            continue
        fi

        if [[ "$line" == "Register A:"* ]]; then
            A=$(echo "$line" | cut -d ':' -f 2 | tr -d '[:space:]')
        elif [[ "$line" == "Register B:"* ]]; then
            B=$(echo "$line" | cut -d ':' -f 2 | tr -d '[:space:]')
        elif [[ "$line" == "Register C:"* ]]; then
            C=$(echo "$line" | cut -d ':' -f 2 | tr -d '[:space:]')
        elif [[ "$line" == "Program:"* ]]; then
            local prog_str
            prog_str=$(echo "$line" | cut -d ':' -f 2 | tr -d '[:space:]')
            IFS=',' read -r -a program <<< "$prog_str"
        fi
    done < "input.txt"

    # Execute the program
    local ip=0
    local opcode
    local operand
    local den
    local val

    while (( ip < ${#program[@]} )); do
        opcode=${program[ip]}
        # Ensure operand exists
        if (( ip + 1 >= ${#program[@]} )); then
            break
        fi
        operand=${program[ip + 1]}

        case $opcode in
            0)
                den=$(get_combo_val "$operand")
                if (( den == 0 )); then
                    A=0
                else
                    A=$(( A >> den )) # A // (1 << den) is equivalent to A >> den
                fi
                ip=$(( ip + 2 ))
                ;;
            1)
                B=$(( B ^ operand ))
                ip=$(( ip + 2 ))
                ;;
            2)
                val=$(get_combo_val "$operand")
                B=$(( val % 8 ))
                ip=$(( ip + 2 ))
                ;;
            3)
                if (( A != 0 )); then
                    ip=$operand
                else
                    ip=$(( ip + 2 ))
                fi
                ;;
            4)
                B=$(( B ^ C ))
                ip=$(( ip + 2 ))
                ;;
            5)
                val=$(get_combo_val "$operand")
                output_vals+=("$(( val % 8 ))")
                ip=$(( ip + 2 ))
                ;;
            6)
                val=$(get_combo_val "$operand")
                # Avoid division by zero shift (though >> 0 is identity)
                 if (( val >= 0 )); then
                     B=$(( A >> val ))
                 fi
                ip=$(( ip + 2 ))
                ;;
            7)
                val=$(get_combo_val "$operand")
                # Avoid division by zero shift
                 if (( val >= 0 )); then
                     C=$(( A >> val ))
                 fi
                ip=$(( ip + 2 ))
                ;;
            *)
                # Unknown opcode, terminate
                break
                ;;
        esac
    done

    # Print the output values, comma-separated
    local IFS=,
    echo "${output_vals[*]}"
}

# Run the main function
main
