
#!/usr/bin/awk -f

# Bitwise AND implementation for standard AWK
function band(a, b,   res, i, p) {
    res = 0; p = 1
    a = int(a); b = int(b)
    for (i = 0; i < 32; i++) {
        if (a % 2 == 1 && b % 2 == 1) res += p
        a = int(a / 2); b = int(b / 2); p *= 2
        if (a == 0 || b == 0) break
    }
    return res
}

# Bitwise OR implementation for standard AWK
function bor(a, b,   res, i, p) {
    res = 0; p = 1
    a = int(a); b = int(b)
    for (i = 0; i < 32; i++) {
        if (a % 2 == 1 || b % 2 == 1) res += p
        a = int(a / 2); b = int(b / 2); p *= 2
        if (a == 0 && b == 0) break
    }
    return res
}

# Verifies if the behavior of an opcode matches the provided sample
function verify_opcode(op, a, b, c, bef, aft,    res, k) {
    # According to the rules, no register other than C should change
    for (k = 0; k < 4; k++) {
        if (k != c && bef[k] != aft[k]) return 0
    }

    # Calculate result based on the instruction type
    if (op == "addr")      res = bef[a] + bef[b]
    else if (op == "addi") res = bef[a] + b
    else if (op == "mulr") res = bef[a] * bef[b]
    else if (op == "muli") res = bef[a] * b
    else if (op == "banr") res = band(bef[a], bef[b])
    else if (op == "bani") res = band(bef[a], b)
    else if (op == "borr") res = bor(bef[a], bef[b])
    else if (op == "bori") res = bor(bef[a], b)
    else if (op == "setr") res = bef[a]
    else if (op == "seti") res = a
    else if (op == "gtir") res = (a > bef[b] ? 1 : 0)
    else if (op == "gtri") res = (bef[a] > b ? 1 : 0)
    else if (op == "gtrr") res = (bef[a] > bef[b] ? 1 : 0)
    else if (op == "eqir") res = (a == bef[b] ? 1 : 0)
    else if (op == "eqri") res = (bef[a] == b ? 1 : 0)
    else if (op == "eqrr") res = (bef[a] == bef[b] ? 1 : 0)
    else return 0

    return (res == aft[c])
}

BEGIN {
    # Ensure we read from input.txt by default
    if (ARGC == 1) {
        ARGV[1] = "input.txt"
        ARGC = 2
    }
    # List of all 16 possible opcodes defined in the manual
    split("addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr", op_names)
    ans = 0
}

# Process blocks representing CPU monitor samples
/^Before:/ {
    # Extract register values from the "Before" line
    split($0, tmp, /[^0-9]+/)
    before[0] = tmp[2]; before[1] = tmp[3]; before[2] = tmp[4]; before[3] = tmp[5]

    # Read the instruction line (opcode A B C)
    if (getline <= 0) exit
    split($0, inst)
    A = inst[2]; B = inst[3]; C = inst[4]

    # Extract register values from the "After" line
    if (getline <= 0) exit
    split($0, tmp, /[^0-9]+/)
    after[0] = tmp[2]; after[1] = tmp[3]; after[2] = tmp[4]; after[3] = tmp[5]

    # Determine how many opcodes are consistent with this sample's behavior
    matches = 0
    for (i = 1; i <= 16; i++) {
        if (verify_opcode(op_names[i], A, B, C, before, after)) {
            matches++
        }
    }

    # Increment global answer if the sample behaves like 3 or more opcodes
    if (matches >= 3) ans++
}

END {
    # Print the total count of samples behaving like 3 or more opcodes
    print ans + 0
}
