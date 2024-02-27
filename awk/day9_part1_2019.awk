
#!/usr/bin/awk -f

BEGIN {
    FS = ","
    while (getline < "input.txt" > 0) {
        for (i = 1; i <= NF; i++) {
            program[i-1] = $i
        }
    }
    close("input.txt")

    for (i = 0; i < length(program); i++) {
        memory[i] = program[i]
    }

    print runIntcode()
}

function getParam(offset) {
    mode = 0
    if (length(modes) >= offset) {
        mode = substr(modes, length(modes)-offset+1, 1)
    }

    param = memory[ip+offset]
    if (mode == 0) {
        return memory[param]
    } else if (mode == 1) {
        return param
    } else if (mode == 2) {
        return memory[relativeBase+param]
    } else {
        print "unknown parameter mode"
        exit 1
    }
}

function setParam(offset, value) {
    mode = 0
    if (length(modes) >= offset) {
        mode = substr(modes, length(modes)-offset+1, 1)
    }

    param = memory[ip+offset]
    if (mode == 0) {
        memory[param] = value
    } else if (mode == 2) {
        memory[relativeBase+param] = value
    } else {
        print "unknown parameter mode"
        exit 1
    }
}

function runIntcode() {
    output = 0
    ip = 0
    relativeBase = 0

    while (1) {
        opcode = memory[ip] % 100
        modes = int(memory[ip] / 100)

        if (opcode == 1) {
            setParam(3, getParam(1)+getParam(2))
            ip += 4
        } else if (opcode == 2) {
            setParam(3, getParam(1)*getParam(2))
            ip += 4
        } else if (opcode == 3) {
            setParam(1, 1) # Test mode input
            ip += 2
        } else if (opcode == 4) {
            output = getParam(1)
            ip += 2
        } else if (opcode == 5) {
            if (getParam(1) != 0) {
                ip = getParam(2)
            } else {
                ip += 3
            }
        } else if (opcode == 6) {
            if (getParam(1) == 0) {
                ip = getParam(2)
            } else {
                ip += 3
            }
        } else if (opcode == 7) {
            if (getParam(1) < getParam(2)) {
                setParam(3, 1)
            } else {
                setParam(3, 0)
            }
            ip += 4
        } else if (opcode == 8) {
            if (getParam(1) == getParam(2)) {
                setParam(3, 1)
            } else {
                setParam(3, 0)
            }
            ip += 4
        } else if (opcode == 9) {
            relativeBase += getParam(1)
            ip += 2
        } else if (opcode == 99) {
            return output
        } else {
            print "unknown opcode: " opcode
            exit 1
        }
    }
}
