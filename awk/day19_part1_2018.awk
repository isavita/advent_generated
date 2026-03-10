
#!/usr/bin/awk -f

# Day 19: Go With The Flow
# This program simulates a virtual machine with 6 registers and an instruction pointer (IP).
# The IP can be bound to a specific register, allowing for relative and absolute jumps.

BEGIN {
    # Ensure the program reads from input.txt if no file is specified as a command-line argument.
    if (ARGC < 2) {
        ARGV[1] = "input.txt"
        ARGC = 2
    }
    # Initialize instruction counter.
    count = 0
}

# The first line of the file defines the register to which the instruction pointer is bound.
# Example: #ip 0
/^#ip/ {
    ip_reg = $2
    next
}

# Instruction lines contain: <opcode> <A> <B> <C>
# Example: seti 5 0 1
NF == 4 {
    ops[count] = $1
    A[count] = $2
    B[count] = $3
    C[count] = $4
    count++
}

END {
    # Initialize the six registers (0 through 5) to 0.
    for (i = 0; i < 6; i++) {
        regs[i] = 0
    }

    # The instruction pointer starts at 0.
    ip = 0

    # The simulation runs until the instruction pointer goes outside the program's bounds.
    while (ip >= 0 && ip < count) {
        # 1. Write the current IP value to the bound register.
        regs[ip_reg] = ip

        # Fetch instruction parts for the current instruction pointer.
        opcode = ops[ip]
        a = A[ip]
        b = B[ip]
        c = C[ip]

        # 2. Execute the instruction based on the opcode name.
        if (opcode == "addr")      regs[c] = regs[a] + regs[b]
        else if (opcode == "addi") regs[c] = regs[a] + b
        else if (opcode == "mulr") regs[c] = regs[a] * regs[b]
        else if (opcode == "muli") regs[c] = regs[a] * b
        else if (opcode == "banr") regs[c] = and(regs[a], regs[b])
        else if (opcode == "bani") regs[c] = and(regs[a], b)
        else if (opcode == "borr") regs[c] = or(regs[a], regs[b])
        else if (opcode == "bori") regs[c] = or(regs[a], b)
        else if (opcode == "setr") regs[c] = regs[a]
        else if (opcode == "seti") regs[c] = a
        else if (opcode == "gtir") regs[c] = (a > regs[b] ? 1 : 0)
        else if (opcode == "gtri") regs[c] = (regs[a] > b ? 1 : 0)
        else if (opcode == "gtrr") regs[c] = (regs[a] > regs[b] ? 1 : 0)
        else if (opcode == "eqir") regs[c] = (a == regs[b] ? 1 : 0)
        else if (opcode == "eqri") regs[c] = (regs[a] == b ? 1 : 0)
        else if (opcode == "eqrr") regs[c] = (regs[a] == regs[b] ? 1 : 0)

        # 3. Write the value of the bound register back to the instruction pointer.
        ip = regs[ip_reg]

        # 4. Increment the instruction pointer to move to the next instruction.
        ip++
    }

    # Output the final value left in register 0 when the program halts.
    print regs[0]
}

