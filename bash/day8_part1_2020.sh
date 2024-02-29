
#!/bin/bash

instructions=()
accumulator=0

while IFS= read -r line
do
    instructions+=("$line")
done < input.txt

executeBootCode() {
    accumulator=0
    declare -A visited
    currentInstruction=0

    while [ $currentInstruction -lt ${#instructions[@]} ]
    do
        if [ ${visited[$currentInstruction]+_} ]
        then
            echo $accumulator
            exit 0
        fi

        visited[$currentInstruction]=1
        IFS=' ' read -r op arg <<< ${instructions[$currentInstruction]}

        case $op in
            "acc")
                accumulator=$((accumulator + arg))
                currentInstruction=$((currentInstruction + 1))
                ;;
            "jmp")
                currentInstruction=$((currentInstruction + arg))
                ;;
            "nop")
                currentInstruction=$((currentInstruction + 1))
                ;;
        esac
    done

    echo $accumulator
}

executeBootCode
