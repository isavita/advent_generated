
#!/bin/bash

# Read the Intcode program from input.txt
IFS=',' read -r -a mem < input.txt

# Initial VM state
ip=0
rel=0
in_ptr=0

# Define the springdroid commands for Part 2 (RUN mode)
# Logic: J = (!A | !B | !C) & D & (E | H)
input="NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
NOT A T
AND A T
OR E T
OR H T
AND T J
RUN
"

# Convert input string to an array of ASCII values
in_buf=($(printf '%s' "$input" | od -An -v -t u1))

# Main Intcode VM loop
while :; do
    (( instr = mem[ip] ))
    (( op = instr % 100 ))
    
    case $op in
        1|2|7|8)
            (( m1 = instr / 100 % 10, m2 = instr / 1000 % 10, m3 = instr / 10000 % 10 ))
            case $m1 in 0) p1=${mem[${mem[ip+1]}]:-0};; 1) p1=${mem[ip+1]};; 2) p1=${mem[${mem[ip+1]}+rel]:-0};; esac
            case $m2 in 0) p2=${mem[${mem[ip+2]}]:-0};; 1) p2=${mem[ip+2]};; 2) p2=${mem[${mem[ip+2]}+rel]:-0};; esac
            case $m3 in 0) a3=${mem[ip+3]};; 2) a3=$((${mem[ip+3]}+rel));; esac
            if (( op == 1 )); then
                (( mem[a3] = p1 + p2 ))
            elif (( op == 2 )); then
                (( mem[a3] = p1 * p2 ))
            elif (( op == 7 )); then
                (( p1 < p2 )) && mem[a3]=1 || mem[a3]=0
            else
                (( p1 == p2 )) && mem[a3]=1 || mem[a3]=0
            fi
            (( ip += 4 ))
            ;;
        3)
            (( m1 = instr / 100 % 10 ))
            case $m1 in 0) a1=${mem[ip+1]};; 2) a1=$((${mem[ip+1]}+rel));; esac
            mem[a1]=${in_buf[in_ptr]:-0}
            (( in_ptr++, ip += 2 ))
            ;;
        4)
            (( m1 = instr / 100 % 10 ))
            case $m1 in 0) p1=${mem[${mem[ip+1]}]:-0};; 1) p1=${mem[ip+1]};; 2) p1=${mem[${mem[ip+1]}+rel]:-0};; esac
            if (( p1 > 127 )); then
                echo $p1
                exit
            fi
            (( ip += 2 ))
            ;;
        5|6)
            (( m1 = instr / 100 % 10, m2 = instr / 1000 % 10 ))
            case $m1 in 0) p1=${mem[${mem[ip+1]}]:-0};; 1) p1=${mem[ip+1]};; 2) p1=${mem[${mem[ip+1]}+rel]:-0};; esac
            case $m2 in 0) p2=${mem[${mem[ip+2]}]:-0};; 1) p2=${mem[ip+2]};; 2) p2=${mem[${mem[ip+2]}+rel]:-0};; esac
            if (( op == 5 )); then
                (( p1 != 0 )) && ip=$p2 || (( ip += 3 ))
            else
                (( p1 == 0 )) && ip=$p2 || (( ip += 3 ))
            fi
            ;;
        9)
            (( m1 = instr / 100 % 10 ))
            case $m1 in 0) p1=${mem[${mem[ip+1]}]:-0};; 1) p1=${mem[ip+1]};; 2) p1=${mem[${mem[ip+1]}+rel]:-0};; esac
            (( rel += p1, ip += 2 ))
            ;;
        99)
            break
            ;;
        *)
            exit 1
            ;;
    esac
done
