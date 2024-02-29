
#!/bin/bash

inputData=($(cat input.txt | tr ',' '\n'))
inputData[1]=12
inputData[2]=2

executeProgram() {
    for ((i=0; i<${#inputData[@]}; i+=4)); do
        pos1=${inputData[i+1]}
        pos2=${inputData[i+2]}
        pos3=${inputData[i+3]}
        case ${inputData[i]} in
            1)
                sum=$((${inputData[pos1]} + ${inputData[pos2]}))
                inputData[pos3]=$sum
                ;;
            2)
                product=$((${inputData[pos1]} * ${inputData[pos2]}))
                inputData[pos3]=$product
                ;;
            99)
                echo ${inputData[0]}
                exit 0
                ;;
            *)
                echo "Invalid opcode"
                exit 1
                ;;
        esac
    done
    echo ${inputData[0]}
}

executeProgram
