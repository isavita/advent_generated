
#!/bin/bash

declare -A data
declare -a out_stream
ip=0
rel_base=0

get_mem() {
    local index=$1
    echo "${data[$index]:-0}"
}

set_mem() {
    local index=$1
    local value=$2
    data[$index]=$value
}

get_param() {
    local offset=$1
    local mode=$2
    local p_val
    p_val=$(get_mem $((ip + offset)))

    case $mode in
        0) # Position Mode
            get_mem "$p_val"
            ;;
        1) # Immediate Mode
            echo "$p_val"
            ;;
        2) # Relative Mode
            get_mem $((rel_base + p_val))
            ;;
        *)
            echo "Error: Invalid parameter mode $mode" >&2
            exit 1
            ;;
    esac
}

get_addr() {
    local offset=$1
    local mode=$2
    local p_val
    p_val=$(get_mem $((ip + offset)))

    case $mode in
        0) # Position Mode
            echo "$p_val"
            ;;
        2) # Relative Mode
            echo $((rel_base + p_val))
            ;;
        *)
            echo "Error: Invalid address mode $mode" >&2
            exit 1
            ;;
    esac
}

run_program() {
    while true; do
        local instruction
        instruction=$(get_mem $ip)
        local opcode=$((instruction % 100))
        local modes=$((instruction / 100))
        local mode1=$((modes % 10))
        local mode2=$(((modes / 10) % 10))
        local mode3=$(((modes / 100) % 10))

        case $opcode in
            1) # ADD
                local val1 val2 addr3
                val1=$(get_param 1 $mode1)
                val2=$(get_param 2 $mode2)
                addr3=$(get_addr 3 $mode3)
                set_mem "$addr3" $((val1 + val2))
                ip=$((ip + 4))
                ;;
            2) # MUL
                local val1 val2 addr3
                val1=$(get_param 1 $mode1)
                val2=$(get_param 2 $mode2)
                addr3=$(get_addr 3 $mode3)
                set_mem "$addr3" $((val1 * val2))
                ip=$((ip + 4))
                ;;
            3) # INPUT
                # This specific problem (Day 13, Part 1) requires no input.
                # If input was needed, it would be read here.
                echo "Error: INPUT opcode used unexpectedly" >&2
                exit 1
                # local addr1=$(get_addr 1 $mode1)
                # read -r input_val # Or get from a predefined source
                # set_mem "$addr1" "$input_val"
                # ip=$((ip + 2))
                ;;
            4) # OUTPUT
                local val1
                val1=$(get_param 1 $mode1)
                out_stream+=("$val1")
                ip=$((ip + 2))
                ;;
            5) # JT (Jump if True)
                local val1 val2
                val1=$(get_param 1 $mode1)
                val2=$(get_param 2 $mode2)
                if [[ $val1 -ne 0 ]]; then
                    ip=$val2
                else
                    ip=$((ip + 3))
                fi
                ;;
            6) # JF (Jump if False)
                local val1 val2
                val1=$(get_param 1 $mode1)
                val2=$(get_param 2 $mode2)
                if [[ $val1 -eq 0 ]]; then
                    ip=$val2
                else
                    ip=$((ip + 3))
                fi
                ;;
            7) # LT (Less Than)
                local val1 val2 addr3
                val1=$(get_param 1 $mode1)
                val2=$(get_param 2 $mode2)
                addr3=$(get_addr 3 $mode3)
                if [[ $val1 -lt $val2 ]]; then
                    set_mem "$addr3" 1
                else
                    set_mem "$addr3" 0
                fi
                ip=$((ip + 4))
                ;;
            8) # EQ (Equals)
                local val1 val2 addr3
                val1=$(get_param 1 $mode1)
                val2=$(get_param 2 $mode2)
                addr3=$(get_addr 3 $mode3)
                if [[ $val1 -eq $val2 ]]; then
                    set_mem "$addr3" 1
                else
                    set_mem "$addr3" 0
                fi
                ip=$((ip + 4))
                ;;
            9) # RBO (Relative Base Offset)
                local val1
                val1=$(get_param 1 $mode1)
                rel_base=$((rel_base + val1))
                ip=$((ip + 2))
                ;;
            99) # HALT
                break
                ;;
            *)
                echo "Error: Unknown opcode $opcode at ip $ip" >&2
                exit 1
                ;;
        esac
    done
}

main() {
    local input_str
    input_str=$(<input.txt)
    IFS=',' read -r -a program <<< "$input_str"

    local i
    for i in "${!program[@]}"; do
        data[$i]=${program[$i]}
    done

    run_program

    local block_count=0
    local idx=0
    while (( idx < ${#out_stream[@]} )); do
        # x=${out_stream[idx]}
        # y=${out_stream[idx+1]}
        local tile_id=${out_stream[idx+2]}
        if [[ $tile_id -eq 2 ]]; then
            ((block_count++))
        fi
        idx=$((idx + 3))
    done

    echo "$block_count"
}

main
