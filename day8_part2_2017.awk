
BEGIN {
    FS = " "
    highestValue = 0
}

{
    reg = $1
    op = $2
    amount = $3
    condReg = $5
    condOp = $6
    condVal = $7

    if (condOp == ">" && registers[condReg] > condVal ||
        condOp == ">=" && registers[condReg] >= condVal ||
        condOp == "<" && registers[condReg] < condVal ||
        condOp == "<=" && registers[condReg] <= condVal ||
        condOp == "==" && registers[condReg] == condVal ||
        condOp == "!=" && registers[condReg] != condVal) {

        if (op == "inc") {
            registers[reg] += amount
        } else if (op == "dec") {
            registers[reg] -= amount
        }

        if (registers[reg] > highestValue) {
            highestValue = registers[reg]
        }
    }
}

END {
    print highestValue
}
