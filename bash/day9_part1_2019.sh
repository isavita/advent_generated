
#!/bin/bash

program=$(cat input.txt | tr ',' ' ')

runIntcode() {
    local output=0 ip=0 relativeBase=0
    while true; do
        opcode=$((memory[ip] % 100))
        modes=$(printf "%d" $((memory[ip] / 100)))

        getParam() {
            local offset=$1 mode=0
            if [ ${#modes} -ge $offset ]; then
                mode=${modes:${#modes}-$offset:1}
            fi

            local param=${memory[ip+offset]}
            case $mode in
                0) echo ${memory[param]} ;;
                1) echo $param ;;
                2) echo ${memory[relativeBase+param]} ;;
                *) echo "unknown parameter mode" && exit 1 ;;
            esac
        }

        setParam() {
            local offset=$1 value=$2 mode=0
            if [ ${#modes} -ge $offset ]; then
                mode=${modes:${#modes}-$offset:1}
            fi

            local param=${memory[ip+offset]}
            case $mode in
                0) memory[$param]=$value ;;
                2) memory[$((relativeBase+param))]=$value ;;
                *) echo "unknown parameter mode" && exit 1 ;;
            esac
        }

        case $opcode in
            1)
                setParam 3 $(( $(getParam 1) + $(getParam 2) ))
                ip=$((ip+4))
                ;;
            2)
                setParam 3 $(( $(getParam 1) * $(getParam 2) ))
                ip=$((ip+4))
                ;;
            3)
                setParam 1 1 # Test mode input
                ip=$((ip+2))
                ;;
            4)
                output=$(getParam 1)
                ip=$((ip+2))
                ;;
            5)
                if [ $(getParam 1) -ne 0 ]; then
                    ip=$(getParam 2)
                else
                    ip=$((ip+3))
                fi
                ;;
            6)
                if [ $(getParam 1) -eq 0 ]; then
                    ip=$(getParam 2)
                else
                    ip=$((ip+3))
                fi
                ;;
            7)
                if [ $(getParam 1) -lt $(getParam 2) ]; then
                    setParam 3 1
                else
                    setParam 3 0
                fi
                ip=$((ip+4))
                ;;
            8)
                if [ $(getParam 1) -eq $(getParam 2) ]; then
                    setParam 3 1
                else
                    setParam 3 0
                fi
                ip=$((ip+4))
                ;;
            9)
                relativeBase=$((relativeBase + $(getParam 1) ))
                ip=$((ip+2))
                ;;
            99)
                echo $output
                exit 0
                ;;
            *)
                echo "unknown opcode: $opcode" && exit 1
                ;;
        esac
    done
}

memory=($program)
runIntcode
