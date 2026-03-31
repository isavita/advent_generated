
#!/bin/bash

# Initialize registers and instruction pointer
a=1
b=0
ip=0

# Pre-parse instructions from input.txt
ops=()
regs=()
offs=()

while read -r l || [[ -n $l ]]; do
    [[ -z $l ]] && continue
    # Replace comma with space and split line into array
    p=(${l//,/ })
    ops+=("${p[0]}")
    if [[ ${p[0]} == "jmp" ]]; then
        regs+=("")
        offs+=("${p[1]}")
    else
        regs+=("${p[1]}")
        offs+=("${p[2]}")
    fi
done < input.txt

n=${#ops[@]}

# Execute the virtual machine
while (( ip >= 0 && ip < n )); do
    o=${ops[ip]}
    r=${regs[ip]}
    v=${offs[ip]}
    
    case $o in
        hlf) (( $r /= 2, ip++ )) ;;
        tpl) (( $r *= 3, ip++ )) ;;
        inc) (( $r++, ip++ )) ;;
        jmp) (( ip += v )) ;;
        jie) (( ip += ( ${!r} % 2 == 0 ? v : 1 ) )) ;;
        jio) (( ip += ( ${!r} == 1 ? v : 1 ) )) ;;
    esac
done

# Print final value of register b
echo $b
