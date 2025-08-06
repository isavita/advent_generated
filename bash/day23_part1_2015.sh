
#!/usr/bin/env bash

# Read instructions from input.txt
declare -a instr reg offset
while IFS= read -r line || [[ -n $line ]]; do
    # Replace commas with spaces, trim leading/trailing spaces
    line=${line//,/ }
    line=${line#"${line%%[! ]*}"}   # ltrim
    line=${line%"${line##*[! ]}"}   # rtrim
    [[ -z $line ]] && continue

    read -ra parts <<< "$line"
    instr+=("${parts[0]}")
    case "${parts[0]}" in
        hlf|tpl|inc)
            reg+=("${parts[1]}")
            offset+=(0)
            ;;
        jmp)
            reg+=(" ")
            offset+=("${parts[1]}")
            ;;
        jie|jio)
            reg+=("${parts[1]}")
            offset+=("${parts[2]}")
            ;;
    esac
done < input.txt

count=${#instr[@]}
a=0
b=0
pc=0

while (( pc >= 0 && pc < count )); do
    case "${instr[pc]}" in
        hlf)
            if [[ ${reg[pc]} == a ]]; then
                ((a /= 2))
            else
                ((b /= 2))
            fi
            ((pc++))
            ;;
        tpl)
            if [[ ${reg[pc]} == a ]]; then
                ((a *= 3))
            else
                ((b *= 3))
            fi
            ((pc++))
            ;;
        inc)
            if [[ ${reg[pc]} == a ]]; then
                ((a++))
            else
                ((b++))
            fi
            ((pc++))
            ;;
        jmp)
            ((pc += offset[pc]))
            ;;
        jie)
            if [[ ${reg[pc]} == a ]]; then
                val=$a
            else
                val=$b
            fi
            if (( val % 2 == 0 )); then
                ((pc += offset[pc]))
            else
                ((pc++))
            fi
            ;;
        jio)
            if [[ ${reg[pc]} == a ]]; then
                val=$a
            else
                val=$b
            fi
            if (( val == 1 )); then
                ((pc += offset[pc]))
            else
                ((pc++))
            fi
            ;;
    esac
done

echo "$b"
