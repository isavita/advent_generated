
#!/usr/bin/awk -f

BEGIN {
    FS = ","
    # AWK arrays are 1-based, but Intcode uses 0-based addressing.
    # We'll read into a temporary 1-based array and copy to a 0-based one.
    # Alternatively, adjust all addressing logic, but copying is simpler here.
    getline < "input.txt"
    n_split = split($0, tmp_prog, FS)
    for (i = 1; i <= n_split; i++) {
        prog[i-1] = tmp_prog[i]
    }
    n = n_split # Store the number of elements for bounds checking if needed
    close("input.txt")

    # Define the main function equivalent
    main()
}

function get_value(mode, param_val,    addr) {
    if (mode == 0) { # Position Mode
        addr = param_val
        return prog[addr]
    } else { # Immediate Mode (mode == 1)
        return param_val
    }
}

function run_program(input_val,    i, instruction, opcode, mode1, mode2, p1, p2, p3, val1, val2) {
    i = 0
    diagnostic_code = 0 # Default or last output

    while (prog[i] != 99) {
        instruction = prog[i]
        opcode = instruction % 100
        mode1 = int(instruction / 100) % 10
        mode2 = int(instruction / 1000) % 10
        # mode3 = int(instruction / 10000) % 10 # Not needed for write ops

        if (opcode == 1) { # Add
            p1 = prog[i+1]
            p2 = prog[i+2]
            p3 = prog[i+3] # Write address (always position mode interpretation)
            val1 = get_value(mode1, p1)
            val2 = get_value(mode2, p2)
            prog[p3] = val1 + val2
            i += 4
        } else if (opcode == 2) { # Multiply
            p1 = prog[i+1]
            p2 = prog[i+2]
            p3 = prog[i+3] # Write address
            val1 = get_value(mode1, p1)
            val2 = get_value(mode2, p2)
            prog[p3] = val1 * val2
            i += 4
        } else if (opcode == 3) { # Input
            p1 = prog[i+1] # Write address
            prog[p1] = input_val
            i += 2
        } else if (opcode == 4) { # Output
            p1 = prog[i+1]
            diagnostic_code = get_value(mode1, p1) # Store the output
            i += 2
        } else {
            # print "Unknown opcode: " opcode " at index " i > "/dev/stderr"
            break # Halt on unknown opcode
        }
        # Basic bounds check (optional based on problem constraints)
        # if (i < 0 || i >= n) {
        #     print "Instruction pointer out of bounds: " i > "/dev/stderr"
        #     break
        # }
    }
    return diagnostic_code
}

function main() {
    result = run_program(1) # Input value is 1
    print result
}
