
#!/usr/bin/env bash

# Read input
ipRegister=0
declare -a op a b c
progLen=0

while IFS= read -r line; do
    [[ -z $line ]] && continue
    if [[ $line == \#ip* ]]; then
        ipRegister=${line#* }
        continue
    fi
    read -r opcode aVal bVal cVal <<< "$line"
    op[progLen]=$opcode
    a[progLen]=$aVal
    b[progLen]=$bVal
    c[progLen]=$cVal
    ((progLen++))
done < input.txt

# Registers
declare -a reg
reg[0]=1 reg[1]=0 reg[2]=0 reg[3]=0 reg[4]=0 reg[5]=0

# Execute program
ip=0
while (( ip >= 0 && ip < progLen )); do
    reg[$ipRegister]=$ip
    opcode=${op[$ip]}
    aVal=${a[$ip]}
    bVal=${b[$ip]}
    cVal=${c[$ip]}
    case $opcode in
        addr) res=$(( reg[aVal] + reg[bVal] )) ;;
        addi) res=$(( reg[aVal] + bVal )) ;;
        mulr) res=$(( reg[aVal] * reg[bVal] )) ;;
        muli) res=$(( reg[aVal] * bVal )) ;;
        banr) res=$(( reg[aVal] & reg[bVal] )) ;;
        bani) res=$(( reg[aVal] & bVal )) ;;
        borr) res=$(( reg[aVal] | reg[bVal] )) ;;
        bori) res=$(( reg[aVal] | bVal )) ;;
        setr) res=$(( reg[aVal] )) ;;
        seti) res=$(( aVal )) ;;
        gtir) res=$(( aVal > reg[bVal] ? 1 : 0 )) ;;
        gtri) res=$(( reg[aVal] > bVal ? 1 : 0 )) ;;
        gtrr) res=$(( reg[aVal] > reg[bVal] ? 1 : 0 )) ;;
        eqir) res=$(( aVal == reg[bVal] ? 1 : 0 )) ;;
        eqri) res=$(( reg[aVal] == bVal ? 1 : 0 )) ;;
        eqrr) res=$(( reg[aVal] == reg[bVal] ? 1 : 0 )) ;;
        *) echo "Unknown opcode: $opcode" >&2; exit 1 ;;
    esac
    reg[$cVal]=$res
    ip=$(( reg[$ipRegister] + 1 ))
done

# Find max register
maxReg=${reg[0]}
for r in "${reg[@]}"; do
    (( r > maxReg )) && maxReg=$r
done

# Sum divisors of maxReg
sum=0
for ((i=1; i<=maxReg; i++)); do
    (( maxReg % i == 0 )) && (( sum += i ))
done

echo "$sum"
