
# AWK Intcode Interpreter

function get_mem(addr,        v) {
    v = memory[addr]
    return v + 0 # Treat non-existent as 0
}

function set_mem(addr, value) {
    memory[addr] = value
}

function get_param(offset, mode,   param, addr) {
    param = get_mem(ip + offset)
    if (mode == 0) { # Position mode
        addr = param
        return get_mem(addr)
    } else if (mode == 1) { # Immediate mode
        return param
    } else if (mode == 2) { # Relative mode
        addr = relative_base + param
        return get_mem(addr)
    } else {
        print "Error: Unknown parameter mode " mode > "/dev/stderr"
        exit 1
    }
}

function get_param_addr(offset, mode,   param, addr) {
     param = get_mem(ip + offset)
     if (mode == 0) { # Position mode
         addr = param
         return addr
     } else if (mode == 2) { # Relative mode
         addr = relative_base + param
         return addr
     } else {
         print "Error: Invalid parameter mode " mode " for write target" > "/dev/stderr"
         exit 1
     }
}

function run_intcode(input_val,   opcode, instruction, modes, mode1, mode2, mode3, p1, p2, p3, addr, val) {
    ip = 0
    relative_base = 0
    latest_output = 0

    while (1) {
        instruction = get_mem(ip)
        opcode = instruction % 100
        modes = int(instruction / 100)
        mode1 = modes % 10
        mode2 = int(modes / 10) % 10
        mode3 = int(modes / 100) % 10

        if (opcode == 1) { # Add
            p1 = get_param(1, mode1)
            p2 = get_param(2, mode2)
            addr = get_param_addr(3, mode3)
            set_mem(addr, p1 + p2)
            ip += 4
        } else if (opcode == 2) { # Multiply
            p1 = get_param(1, mode1)
            p2 = get_param(2, mode2)
            addr = get_param_addr(3, mode3)
            set_mem(addr, p1 * p2)
            ip += 4
        } else if (opcode == 3) { # Input
            addr = get_param_addr(1, mode1)
            set_mem(addr, input_val)
            ip += 2
        } else if (opcode == 4) { # Output
            latest_output = get_param(1, mode1)
            ip += 2
        } else if (opcode == 5) { # Jump-if-true
            p1 = get_param(1, mode1)
            p2 = get_param(2, mode2)
            if (p1 != 0) {
                ip = p2
            } else {
                ip += 3
            }
        } else if (opcode == 6) { # Jump-if-false
            p1 = get_param(1, mode1)
            p2 = get_param(2, mode2)
            if (p1 == 0) {
                ip = p2
            } else {
                ip += 3
            }
        } else if (opcode == 7) { # Less than
            p1 = get_param(1, mode1)
            p2 = get_param(2, mode2)
            addr = get_param_addr(3, mode3)
            set_mem(addr, (p1 < p2) ? 1 : 0)
            ip += 4
        } else if (opcode == 8) { # Equals
            p1 = get_param(1, mode1)
            p2 = get_param(2, mode2)
            addr = get_param_addr(3, mode3)
            set_mem(addr, (p1 == p2) ? 1 : 0)
            ip += 4
        } else if (opcode == 9) { # Adjust relative base
            relative_base += get_param(1, mode1)
            ip += 2
        } else if (opcode == 99) { # Halt
            return latest_output
        } else {
            print "Error: Unknown opcode " opcode " at ip " ip > "/dev/stderr"
            exit 1
        }
    }
}

# Main entry point
BEGIN {
    FS=","
    if ((getline line < "input.txt") <= 0) {
         print "Error: Cannot read input.txt" > "/dev/stderr"
         exit 1
    }
    close("input.txt")

    n = split(line, p, ",")
    delete memory # Clear memory array
    for (i = 1; i <= n; i++) {
        memory[i-1] = p[i] # Use 0-based indexing
    }

    result = run_intcode(2) # Provide input value 2
    print result
    exit
}

