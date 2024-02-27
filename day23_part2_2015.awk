
{
    lines[NR] = $0
}

END {
    split(lines[1], parts, " ")
    registers["a"] = 1
    registers["b"] = 0

    for (i = 1; i <= length(lines); i++) {
        split(lines[i], parts, " ")

        if (parts[1] == "hlf") {
            registers[parts[2]] /= 2
        } else if (parts[1] == "tpl") {
            registers[parts[2]] *= 3
        } else if (parts[1] == "inc") {
            registers[parts[2]]++
        } else if (parts[1] == "jmp") {
            i += parts[2] - 1
        } else if (parts[1] == "jie") {
            if (registers[substr(parts[2], 1, 1)] % 2 == 0) {
                i += parts[3] - 1
            }
        } else if (parts[1] == "jio") {
            if (registers[substr(parts[2], 1, 1)] == 1) {
                i += parts[3] - 1
            }
        } else {
            print "Unknown instruction: " parts[1]
            exit 1
        }
    }

    print registers["b"]
}
