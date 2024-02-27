
#!/usr/bin/awk -f

BEGIN {
    while (getline < "input.txt") {
        instructions[++n] = $0
    }
    
    a = 1
    while (1) {
        if (producesClockSignal(a, instructions)) {
            print a
            exit
        }
        a++
    }
}

function producesClockSignal(a, instructions) {
    delete registers
    registers["a"] = a
    registers["b"] = 0
    registers["c"] = 0
    registers["d"] = 0
    
    outputCount = 0
    lastOutput = 0
    
    i = 1
    while (i <= n) {
        split(instructions[i], parts, " ")
        instruction = parts[1]
        if (instruction == "cpy") {
            val = getValue(parts[2], registers)
            registers[parts[3]] = val
        } else if (instruction == "inc") {
            registers[parts[2]]++
        } else if (instruction == "dec") {
            registers[parts[2]]--
        } else if (instruction == "jnz") {
            val = getValue(parts[2], registers)
            if (val != 0) {
                i += parts[3]
                continue
            }
        } else if (instruction == "out") {
            val = getValue(parts[2], registers)
            if (val != 0 && val != 1) {
                return 0
            }
            if (outputCount > 0 && val == lastOutput) {
                return 0
            }
            lastOutput = val
            outputCount++
            if (outputCount > 50) {
                return 1
            }
        }
        i++
    }
    return 0
}

function getValue(s, registers) {
    if (s ~ /^[0-9]+$/) {
        return s
    } else {
        return registers[s]
    }
}
