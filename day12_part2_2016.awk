
{
    instructions[NR] = $0
}

END {
    registers["a"] = 0
    registers["b"] = 0
    registers["c"] = 1
    registers["d"] = 0

    for (i = 1; i <= NR; ) {
        split(instructions[i], parts, " ")
        if (parts[1] == "cpy") {
            val = (parts[2] ~ /[a-d]/) ? registers[parts[2]] : parts[2]
            registers[parts[3]] = val
            i++
        } else if (parts[1] == "inc") {
            registers[parts[2]]++
            i++
        } else if (parts[1] == "dec") {
            registers[parts[2]]--
            i++
        } else if (parts[1] == "jnz") {
            val = (parts[2] ~ /[a-d]/) ? registers[parts[2]] : parts[2]
            if (val != 0) {
                i += parts[3]
            } else {
                i++
            }
        }
    }

    print registers["a"]
}
