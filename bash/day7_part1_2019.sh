#!/bin/bash

# Reads the input file and stores the content in a global variable
read_input() {
    IFS=',' read -r -a code < "input.txt"
}

# Simulates the VM execution with input and output through passed parameters
run_vm() {
    local ip=0
    local opcode
    local input=("$@")
    local input_idx=0
    local output

    while true; do
        opcode=$(( code[ip] % 100 ))
        mode1=$(( (code[ip] / 100) % 10 ))
        mode2=$(( (code[ip] / 1000) % 10 ))
        mode3=$(( (code[ip] / 10000) % 10 ))

        param1=$(( mode1 == 1 ? code[ip+1] : code[code[ip+1]] ))
        param2=$(( mode2 == 1 ? code[ip+2] : code[code[ip+2]] ))
        param3=$(( mode3 == 1 ? code[ip+3] : code[code[ip+3]] ))

        case $opcode in
            1) # add
                code[code[ip+3]]=$(( param1 + param2 ))
                ip=$(( ip + 4 ))
                ;;
            2) # multiply
                code[code[ip+3]]=$(( param1 * param2 ))
                ip=$(( ip + 4 ))
                ;;
            3) # input
                code[code[ip+1]]=${input[$input_idx]}
                input_idx=$(( input_idx + 1 ))
                ip=$(( ip + 2 ))
                ;;
            4) # output
                output=$param1
                ip=$(( ip + 2 ))
                ;;
            5) # jump-if-true
                if [ $param1 -ne 0 ]; then
                    ip=$param2
                else
                    ip=$(( ip + 3 ))
                fi
                ;;
            6) # jump-if-false
                if [ $param1 -eq 0 ]; then
                    ip=$param2
                else
                    ip=$(( ip + 3 ))
                fi
                ;;
            7) # less than
                if [ $param1 -lt $param2 ]; then
                    code[code[ip+3]]=1
                else
                    code[code[ip+3]]=0
                fi
                ip=$(( ip + 4 ))
                ;;
            8) # equals
                if [ $param1 -eq $param2 ]; then
                    code[code[ip+3]]=1
                else
                    code[code[ip+3]]=0
                fi
                ip=$(( ip + 4 ))
                ;;
            99) # halt
                echo $output
                return
                ;;
            *)
                echo "Unknown opcode: $opcode"
                exit 1
                ;;
        esac
    done
}

# Generates permutations of input array and stores them in a global array
permute() {
    local items=("$@")
    local i
    if [ ${#items[@]} -eq 1 ]; then
        echo "${items[@]}"
    else
        for i in $(seq 0 $(( ${#items[@]} - 1 ))); do
            local left=("${items[@]:0:i}" "${items[@]:i+1}")
            for p in $(permute "${left[@]}"); do
                echo "${items[i]},$p"
            done
        done
    fi
}

main() {
    read_input

    local max=0
    for phase in $(permute 0 1 2 3 4); do
        IFS=',' read -r -a phases <<< "$phase"
        local input=0
        for i in {0..4}; do
            input=$(run_vm ${phases[i]} $input)
        done
        if (( input > max )); then
            max=$input
        fi
    done
    echo "Max signal: $max"
}

main "$@"