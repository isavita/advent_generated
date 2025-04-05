
#!/bin/bash

declare -A code
ip=0
relative_base=0
declare -a input_queue
declare -a output_queue

load_code() {
    local file="$1"
    local i=0
    local line
    IFS=',' read -r -a line < "$file"
    for val in "${line[@]}"; do
        code[$i]=$val
        ((i++))
    done
}

get_val() {
    local addr=$1
    echo "${code[$addr]:-0}"
}

get_param() {
    local index=$1
    local modes=$2
    local mode=$(( modes % 10 ))
    local val_at_ip_plus_index
    val_at_ip_plus_index=$(get_val $((ip + index)))

    case $mode in
        0) echo "$(get_val "$val_at_ip_plus_index")" ;; # Position mode
        1) echo "$val_at_ip_plus_index" ;;              # Immediate mode
        2) echo "$(get_val $((relative_base + val_at_ip_plus_index)))" ;; # Relative mode
    esac
}

get_address() {
    local index=$1
    local modes=$2
    local mode=$(( modes % 10 ))
    local val_at_ip_plus_index
    val_at_ip_plus_index=$(get_val $((ip + index)))

    case $mode in
        0) echo "$val_at_ip_plus_index" ;;              # Position mode
        2) echo "$((relative_base + val_at_ip_plus_index))" ;; # Relative mode
    esac
}

run_vm() {
    while true; do
        local cmd opcode modes p1 p2 p3 addr jump_addr ip_inc
        cmd=$(get_val $ip)
        opcode=$(( cmd % 100 ))
        modes=$(( cmd / 100 ))
        ip_inc=0

        case $opcode in
            1) # add
                p1=$(get_param 1 $modes)
                p2=$(get_param 2 $((modes / 10)))
                addr=$(get_address 3 $((modes / 100)))
                code[$addr]=$(( p1 + p2 ))
                ip_inc=4
                ;;
            2) # multiply
                p1=$(get_param 1 $modes)
                p2=$(get_param 2 $((modes / 10)))
                addr=$(get_address 3 $((modes / 100)))
                code[$addr]=$(( p1 * p2 ))
                ip_inc=4
                ;;
            3) # read
                addr=$(get_address 1 $modes)
                code[$addr]=${input_queue[0]}
                input_queue=("${input_queue[@]:1}")
                ip_inc=2
                ;;
            4) # write
                p1=$(get_param 1 $modes)
                output_queue+=("$p1")
                ip_inc=2
                ;;
            5) # jump-if-true
                p1=$(get_param 1 $modes)
                p2=$(get_param 2 $((modes / 10)))
                if (( p1 != 0 )); then
                    ip=$p2
                else
                    ip_inc=3
                fi
                ;;
            6) # jump-if-false
                p1=$(get_param 1 $modes)
                p2=$(get_param 2 $((modes / 10)))
                if (( p1 == 0 )); then
                    ip=$p2
                else
                    ip_inc=3
                fi
                ;;
            7) # less than
                p1=$(get_param 1 $modes)
                p2=$(get_param 2 $((modes / 10)))
                addr=$(get_address 3 $((modes / 100)))
                if (( p1 < p2 )); then
                    code[$addr]=1
                else
                    code[$addr]=0
                fi
                ip_inc=4
                ;;
            8) # equals
                p1=$(get_param 1 $modes)
                p2=$(get_param 2 $((modes / 10)))
                addr=$(get_address 3 $((modes / 100)))
                if (( p1 == p2 )); then
                    code[$addr]=1
                else
                    code[$addr]=0
                fi
                ip_inc=4
                ;;
            9) # adjust relative base
                p1=$(get_param 1 $modes)
                relative_base=$(( relative_base + p1 ))
                ip_inc=2
                ;;
            99) # halt
                break
                ;;
            *)
                echo "Unknown opcode: $opcode at ip $ip" >&2
                exit 1
                ;;
        esac
        (( ip += ip_inc ))
    done
}

send_string() {
    local s="$1"
    local i char_val
    for (( i=0; i<${#s}; i++ )); do
        printf -v char_val '%d' "'${s:i:1}"
        input_queue+=("$char_val")
    done
    input_queue+=(10) # Newline
}

main() {
    load_code "input.txt"

    local instructions=(
        "NOT A J"
        "NOT B T"
        "OR T J"
        "NOT C T"
        "OR T J"
        "AND D J"
        "WALK"
    )

    for instruction in "${instructions[@]}"; do
        send_string "$instruction"
    done

    run_vm

    local output
    for output in "${output_queue[@]}"; do
        if (( output > 127 )); then
            echo "$output"
            break
        fi
    done
}

main
