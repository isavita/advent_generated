
#!/usr/bin/awk -f

# Helper function to get value (either register content or literal integer)
function getValue(x, reg,    val) {
    if (x ~ /^[a-d]$/) {
        val = reg[x]
    } else {
        val = x + 0 # Force numeric conversion
    }
    return val
}

# Helper function to check if argument is a valid register
function isReg(x) {
    return (x ~ /^[a-d]$/)
}

# Helper function to toggle an instruction string
function toggle(instr_str,    parts, op, new_instr) {
    split(instr_str, parts, " ")
    op = parts[1]

    if (op == "inc") {
        parts[1] = "dec"
    } else if (op == "dec" || op == "tgl") {
        parts[1] = "inc"
    } else if (op == "jnz") {
        parts[1] = "cpy"
    } else if (op == "cpy") {
        parts[1] = "jnz"
    }

    # Rebuild the instruction string
    new_instr = parts[1]
    for (i = 2; i <= length(parts); ++i) {
        new_instr = new_instr " " parts[i]
    }
    return new_instr
}

BEGIN {
    # Initialize registers
    reg["a"] = 7
    reg["b"] = 0
    reg["c"] = 0
    reg["d"] = 0

    # Read all instructions into an array (AWK arrays are 1-based)
    n = 0
    while (getline line < "input.txt" > 0) {
        instr[++n] = line
    }
    close("input.txt")

    # Main execution loop (emulates the Python while loop)
    pc = 1 # Program counter (1-based index)
    while (pc >= 1 && pc <= n) {
        current_instr = instr[pc]
        split(current_instr, fields, " ")
        op = fields[1]
        arg1 = fields[2]
        arg2 = "" # Initialize arg2
        if (length(fields) >= 3) {
             arg2 = fields[3]
        }

        jumped = 0 # Flag to check if jnz caused a jump

        if (op == "cpy") {
            x = getValue(arg1, reg)
            if (isReg(arg2)) {
                 reg[arg2] = x
            }
        } else if (op == "inc") {
            if (isReg(arg1)) {
                reg[arg1]++
            }
        } else if (op == "dec") {
             if (isReg(arg1)) {
                reg[arg1]--
            }
        } else if (op == "jnz") {
            x = getValue(arg1, reg)
            if (x != 0) {
                y = getValue(arg2, reg)
                pc += y
                jumped = 1 # Mark that we jumped
            }
        } else if (op == "tgl") {
            x = getValue(arg1, reg)
            tgt = pc + x
            if (tgt >= 1 && tgt <= n) {
                 instr[tgt] = toggle(instr[tgt])
            }
        }

        # Increment PC only if jnz didn't jump
        if (!jumped) {
            pc++
        }
    }

    # Print the final result
    print reg["a"]
}
