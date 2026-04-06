
#!/bin/bash

# Parse input.txt
RA=$(grep "Register A" input.txt | awk '{print $3}')
RB=$(grep "Register B" input.txt | awk '{print $3}')
RC=$(grep "Register C" input.txt | awk '{print $3}')
PROG_STR=$(grep "Program" input.txt | cut -d' ' -f2)
IFS=',' read -r -a PROG <<< "$PROG_STR"
PLEN=${#PROG[@]}

run_prog() {
    local a=$1 b=$RB c=$RC ip=0 out=""
    while (( ip < PLEN )); do
        local ins=${PROG[ip]}
        local val=${PROG[ip+1]}
        local combo=$val
        case $val in
            4) combo=$a ;;
            5) combo=$b ;;
            6) combo=$c ;;
        esac
        case $ins in
            0) (( a >>= combo )) ;;
            1) (( b ^= val )) ;;
            2) (( b = combo & 7 )) ;;
            3) if (( a != 0 )); then ip=$val; continue; fi ;;
            4) (( b ^= c )) ;;
            5) out+="$(( combo & 7 )),";;
            6) (( b = a >> combo )) ;;
            7) (( c = a >> combo )) ;;
        esac
        (( ip += 2 ))
    done
    echo "${out%,}"
}

solve() {
    local val=$1
    local step=$2
    if (( step < 0 )); then
        echo "$val"
        return 0
    fi

    local target="${PROG[step]}"
    for (( k=step+1; k<PLEN; k++ )); do
        target+=",${PROG[k]}"
    done

    for d in {0..7}; do
        local curr_a=$(( (val << 3) | d ))
        if (( curr_a == 0 && step == PLEN - 1 )); then continue; fi
        if [[ $(run_prog "$curr_a") == "$target" ]]; then
            local res=$(solve "$curr_a" $((step - 1)))
            if [[ -n "$res" ]]; then
                echo "$res"
                return 0
            fi
        fi
    done
}

# Part 1
run_prog "$RA"

# Part 2
solve 0 $((PLEN - 1))

