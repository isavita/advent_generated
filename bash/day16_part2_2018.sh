
#!/bin/bash

run() {
    local op=$1 A=$2 B=$3 C=$4
    case $op in
        addr) reg[$C]=$((reg[$A] + reg[$B])) ;;
        addi) reg[$C]=$((reg[$A] + B)) ;;
        mulr) reg[$C]=$((reg[$A] * reg[$B])) ;;
        muli) reg[$C]=$((reg[$A] * B)) ;;
        banr) reg[$C]=$((reg[$A] & reg[$B])) ;;
        bani) reg[$C]=$((reg[$A] & B)) ;;
        borr) reg[$C]=$((reg[$A] | reg[$B])) ;;
        bori) reg[$C]=$((reg[$A] | B)) ;;
        setr) reg[$C]=${reg[$A]} ;;
        seti) reg[$C]=$A ;;
        gtir) reg[$C]=$((A > reg[$B] ? 1 : 0)) ;;
        gtri) reg[$C]=$((reg[$A] > B ? 1 : 0)) ;;
        gtrr) reg[$C]=$((reg[$A] > reg[$B] ? 1 : 0)) ;;
        eqir) reg[$C]=$((A == reg[$B] ? 1 : 0)) ;;
        eqri) reg[$C]=$((reg[$A] == B ? 1 : 0)) ;;
        eqrr) reg[$C]=$((reg[$A] == reg[$B] ? 1 : 0)) ;;
    esac
}

check() {
    local op=$1 A=$2 B=$3 C=$4 res
    case $op in
        addr) res=$((before[$A] + before[$B])) ;;
        addi) res=$((before[$A] + B)) ;;
        mulr) res=$((before[$A] * before[$B])) ;;
        muli) res=$((before[$A] * B)) ;;
        banr) res=$((before[$A] & before[$B])) ;;
        bani) res=$((before[$A] & B)) ;;
        borr) res=$((before[$A] | before[$B])) ;;
        bori) res=$((before[$A] | B)) ;;
        setr) res=${before[$A]} ;;
        seti) res=$A ;;
        gtir) res=$((A > before[$B] ? 1 : 0)) ;;
        gtri) res=$((before[$A] > B ? 1 : 0)) ;;
        gtrr) res=$((before[$A] > before[$B] ? 1 : 0)) ;;
        eqir) res=$((A == before[$B] ? 1 : 0)) ;;
        eqri) res=$((before[$A] == B ? 1 : 0)) ;;
        eqrr) res=$((before[$A] == before[$B] ? 1 : 0)) ;;
    esac
    [[ $res -eq ${after[$C]} ]] || return 1
    for i in 0 1 2 3; do
        ((i == C)) && continue
        [[ ${before[$i]} -eq ${after[$i]} ]] || return 1
    done
    return 0
}

p1=0
all_ops="addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr"
declare -a possible
for i in {0..15}; do possible[$i]=$all_ops; done
program=()

while IFS= read -r line || [[ -n "$line" ]]; do
    if [[ $line =~ Before:[[:space:]]*\[([0-9]+),[[:space:]]*([0-9]+),[[:space:]]*([0-9]+),[[:space:]]*([0-9]+)\] ]]; then
        before=(${BASH_REMATCH[1]} ${BASH_REMATCH[2]} ${BASH_REMATCH[3]} ${BASH_REMATCH[4]})
        IFS= read -r line; instr=($line)
        IFS= read -r line; [[ $line =~ After:[[:space:]]*\[([0-9]+),[[:space:]]*([0-9]+),[[:space:]]*([0-9]+),[[:space:]]*([0-9]+)\] ]]
        after=(${BASH_REMATCH[1]} ${BASH_REMATCH[2]} ${BASH_REMATCH[3]} ${BASH_REMATCH[4]})
        count=0
        valid_for_sample=""
        for op in $all_ops; do
            if check "$op" "${instr[1]}" "${instr[2]}" "${instr[3]}"; then
                ((count++))
                valid_for_sample+="$op "
            fi
        done
        ((count >= 3)) && ((p1++))
        new_p=""
        for op in ${possible[${instr[0]}]}; do
            [[ " $valid_for_sample " == *" $op "* ]] && new_p+="$op "
        done
        possible[${instr[0]}]=$new_p
    elif [[ $line =~ [0-9] ]]; then
        program+=("$line")
    fi
done < input.txt

echo $p1

declare -a map
while [[ ${#map[@]} -lt 16 ]]; do
    for i in {0..15}; do
        [[ -n ${map[$i]} ]] && continue
        set -- ${possible[$i]}
        if [[ $# -eq 1 ]]; then
            val=$1
            map[$i]=$val
            for j in {0..15}; do
                possible[$j]=" ${possible[$j]} "
                possible[$j]=${possible[$j]// $val / }
                set -- ${possible[$j]}
                possible[$j]="$*"
            done
        fi
    done
done

reg=(0 0 0 0)
for line in "${program[@]}"; do
    set -- $line
    run "${map[$1]}" "$2" "$3" "$4"
done
echo "${reg[0]}"
