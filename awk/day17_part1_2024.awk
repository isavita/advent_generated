
#!/usr/bin/awk -f

# -------------------------------------------------------------------
# Day 17: Chronospatial Computer
# Solution simulates a 3-bit computer with 8 instructions.
# -------------------------------------------------------------------

BEGIN {
    # Ensure the script reads from input.txt if no file is provided as an argument
    if (ARGC < 2) {
        ARGV[1] = "input.txt"
        ARGC = 2
    }
}

# Parse register initialization values
/^Register A:/ { regA = $3 }
/^Register B:/ { regB = $3 }
/^Register C:/ { regC = $3 }

# Parse the comma-separated program instructions
/^Program:/ {
    line = $0
    sub(/^Program:[[:space:]]*/, "", line)
    prog_len = split(line, prog_raw, ",")
    for (i = 1; i <= prog_len; i++) {
        program[i-1] = prog_raw[i]
    }
}

# Computer execution logic
END {
    ip = 0
    output_str = ""
    
    while (ip < prog_len) {
        opcode = program[ip]
        operand = program[ip+1]
        
        # Determine combo operand value:
        # 0-3: literals, 4: Reg A, 5: Reg B, 6: Reg C, 7: Reserved
        combo = operand
        if (operand == 4)      combo = regA
        else if (operand == 5) combo = regB
        else if (operand == 6) combo = regC
        
        # Instruction Execution
        if (opcode == 0) {        # adv: A = A / 2^combo
            regA = int(regA / (2 ^ combo))
        } else if (opcode == 1) { # bxl: B = B XOR literal_operand
            regB = bxor(regB, operand)
        } else if (opcode == 2) { # bst: B = combo % 8
            regB = combo % 8
        } else if (opcode == 3) { # jnz: jump if A != 0
            if (regA != 0) {
                ip = operand
                continue
            }
        } else if (opcode == 4) { # bxc: B = B XOR C (ignores operand)
            regB = bxor(regB, regC)
        } else if (opcode == 5) { # out: output combo % 8
            val = combo % 8
            output_str = (output_str == "" ? "" : output_str ",") val
        } else if (opcode == 6) { # bdv: B = A / 2^combo
            regB = int(regA / (2 ^ combo))
        } else if (opcode == 7) { # cdv: C = A / 2^combo
            regC = int(regA / (2 ^ combo))
        }
        
        # Increment instruction pointer
        ip += 2
    }
    
    # Print the comma-separated output
    print output_str
}

# Manual bitwise XOR implementation for portability across AWK versions.
# Handles values up to the 53-bit integer limit of IEEE 754 doubles.
function bxor(x, y,   res, bit, i) {
    res = 0
    for (i = 0; i < 53; i++) {
        bit = 2 ^ i
        # Break early if the remaining bits of both numbers are 0
        if (bit > x && bit > y) break
        # If the i-th bits are different, set the i-th bit in the result
        if ((int(x / bit) % 2) != (int(y / bit) % 2)) {
            res += bit
        }
    }
    return res
}

