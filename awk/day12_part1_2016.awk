
# AWK solution
BEGIN {
    while ((getline < "input.txt") > 0) {
        instructions[++n] = $0
    }
    executeInstructions(instructions, registers)
    print registers["a"]
}

function executeInstructions(instructions, registers,    i, parts, val, jump) {
    for (i = 1; i <= length(instructions);) {
        split(instructions[i], parts)
        if (parts[1] == "cpy") {
            val = getValue(parts[2], registers)
            registers[parts[3]] = val
            i++
        } else if (parts[1] == "inc") {
            registers[parts[2]]++
            i++
        } else if (parts[1] == "dec") {
            registers[parts[2]]--
            i++
        } else if (parts[1] == "jnz") {
            val = getValue(parts[2], registers)
            if (val != 0) {
                jump = (parts[3] + 0)
                i += jump
            } else {
                i++
            }
        }
    }
}

function getValue(s, registers) {
    if (s + 0 == s) {
        return s
    } else {
        return registers[s]
    }
}
