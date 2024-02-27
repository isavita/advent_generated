
{
    instructions[NR] = $0
}

function executeBootCode(instructions,       accumulator, visited, currentInstruction, parts, op, arg) {
    accumulator = 0
    delete visited
    currentInstruction = 1

    while (currentInstruction in instructions) {
        if (visited[currentInstruction]) {
            print accumulator
            exit
        }

        visited[currentInstruction] = 1
        split(instructions[currentInstruction], parts, " ")
        op = parts[1]
        arg = parts[2]

        if (op == "acc") {
            accumulator += arg
            currentInstruction++
        } else if (op == "jmp") {
            currentInstruction += arg
        } else if (op == "nop") {
            currentInstruction++
        }
    }

    print accumulator
    exit
}

END {
    executeBootCode(instructions)
}
