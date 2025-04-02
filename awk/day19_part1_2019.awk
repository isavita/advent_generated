
#!/usr/bin/awk -f

# Global variables for VM state
# mem: VM memory (associative array)
# ip: instruction pointer
# rb: relative base
# input_queue: array to hold input values
# input_idx: index for the next input value to read
# output_val: stores the last output value

function get_param(idx,   cmd, mode, val) {
    cmd = mem[ip]
    mode = int(cmd / (10^(idx+1))) % 10
    val = mem[ip + idx]
    if (mode == 0) { # Position mode
        return mem[val]
    } else if (mode == 1) { # Immediate mode
        return val
    } else { # Relative mode (mode == 2)
        return mem[rb + val]
    }
    # AWK returns 0 or "" for non-existent indices, mimicking python .get(key, 0)
}

function get_address(idx,   cmd, mode, val) {
    cmd = mem[ip]
    mode = int(cmd / (10^(idx+1))) % 10
    val = mem[ip + idx]
    if (mode == 0) { # Position mode
        return val
    } else { # Relative mode (mode == 2)
        return rb + val
    }
    # Mode 1 is invalid for address parameters
    print "Error: Invalid mode for address parameter" > "/dev/stderr"
    exit 1
}

function run_vm(   cmd, opcode, p1, p2, addr) {
    # Reset VM state for this run - copy prog to mem
    split("", mem) # Clear mem array
    for (i in prog) {
        mem[i] = prog[i]
    }
    ip = 0
    rb = 0
    input_idx = 1 # AWK arrays are 1-based by default for numeric access
    output_val = -1 # Default output if none produced

    while (1) {
        cmd = mem[ip]
        opcode = cmd % 100

        if (opcode == 1) { # add
            p1 = get_param(1)
            p2 = get_param(2)
            addr = get_address(3)
            mem[addr] = p1 + p2
            ip += 4
        } else if (opcode == 2) { # multiply
            p1 = get_param(1)
            p2 = get_param(2)
            addr = get_address(3)
            mem[addr] = p1 * p2
            ip += 4
        } else if (opcode == 3) { # input
            addr = get_address(1)
            mem[addr] = input_queue[input_idx++]
            ip += 2
        } else if (opcode == 4) { # output
            output_val = get_param(1)
            ip += 2
            # In this specific problem, we only care about the first (and only) output
            # We could potentially 'break' here after getting the output,
            # but the original runs to completion, so we will too.
        } else if (opcode == 5) { # jump-if-true
            p1 = get_param(1)
            p2 = get_param(2)
            if (p1 != 0) {
                ip = p2
            } else {
                ip += 3
            }
        } else if (opcode == 6) { # jump-if-false
            p1 = get_param(1)
            p2 = get_param(2)
            if (p1 == 0) {
                ip = p2
            } else {
                ip += 3
            }
        } else if (opcode == 7) { # less than
            p1 = get_param(1)
            p2 = get_param(2)
            addr = get_address(3)
            mem[addr] = (p1 < p2) ? 1 : 0
            ip += 4
        } else if (opcode == 8) { # equals
            p1 = get_param(1)
            p2 = get_param(2)
            addr = get_address(3)
            mem[addr] = (p1 == p2) ? 1 : 0
            ip += 4
        } else if (opcode == 9) { # adjust relative base
            p1 = get_param(1)
            rb += p1
            ip += 2
        } else if (opcode == 99) { # halt
            break
        } else {
            printf("Unknown opcode %d at ip %d\n", opcode, ip) > "/dev/stderr"
            exit 1
        }
    }
    return output_val
}

function beam(x, y) {
    input_queue[1] = x # Set inputs for the run_vm call
    input_queue[2] = y
    result = run_vm()
    return (result == 1)
}

BEGIN {
    # Read the program from input.txt
    if ((getline < "input.txt") <= 0) {
        print "Error reading input.txt" > "/dev/stderr"
        exit 1
    }
    close("input.txt")

    # Split the line into the initial program memory (prog array)
    split($0, prog_str, ",")
    for (i in prog_str) {
        prog[i-1] = prog_str[i] # Store using 0-based index
    }

    # Main logic equivalent to python main()
    total_sum = 0
    for (y = 0; y < 50; y++) {
        for (x = 0; x < 50; x++) {
            if (beam(x, y)) {
                total_sum++
            }
        }
    }
    print total_sum

    exit # Ensure script exits after BEGIN block
}
