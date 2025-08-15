#!/usr/bin/awk -f
# Coprocessor Conflagration – count how many times `mul` is executed.
# The program reads its instructions from a file named `input.txt`
# and prints the count of `mul` invocations to standard output.

BEGIN {
    # initialise registers a‑h to 0
    for (c = 97; c <= 104; ++c) {
        r = sprintf("%c", c)
        regs[r] = 0
    }

    # -----------------------------------------------------------------
    # Load the program from input.txt
    # -----------------------------------------------------------------
    n = 0
    while ((getline line < "input.txt") > 0) {
        ++n
        split(line, parts, " ")
        op[n] = parts[1]
        arg1[n] = parts[2]
        arg2[n] = parts[3]   # may be empty for future extensions
    }
    close("input.txt")

    # -----------------------------------------------------------------
    # Execute the program, counting `mul` instructions
    # -----------------------------------------------------------------
    pc = 1               # program counter (1‑based)
    mulcnt = 0

    while (pc >= 1 && pc <= n) {
        instr = op[pc]
        x = arg1[pc]
        y = arg2[pc]

        if (instr == "set") {
            regs[x] = val(y)
        } else if (instr == "sub") {
            regs[x] -= val(y)
        } else if (instr == "mul") {
            regs[x] *= val(y)
            ++mulcnt
        } else if (instr == "jnz") {
            if (val(x) != 0) {
                pc += val(y)
                continue
            }
        }
        ++pc
    }

    # Output the result
    print mulcnt
}

# -----------------------------------------------------------------
# Helper: return the numeric value of a token (register or constant)
# -----------------------------------------------------------------
function val(token,   num) {
    if (token ~ /^[a-z]$/)   # a register name
        return regs[token]
    else
        return token + 0     # numeric constant (handles negatives)
}
