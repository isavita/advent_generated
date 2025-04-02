
#!/usr/bin/awk -f

# Global variables
BEGIN {
    # Read the entire program code from input.txt
    if ((getline line < "input.txt") <= 0) {
        print "Error: Cannot read input.txt" > "/dev/stderr"
        exit 1
    }
    close("input.txt")
    n_orig = split(line, tmp_code, ",")
    for (i = 1; i <= n_orig; ++i) {
        original_code[i-1] = tmp_code[i] + 0 # Store 0-based, ensure numeric
    }

    max_output = 0

    # Initialize phases array (0-based)
    phases[0] = 0; phases[1] = 1; phases[2] = 2; phases[3] = 3; phases[4] = 4

    # Generate permutations and test amplifier chains
    generate_permutations(phases, 0, 4)

    # Print the final maximum output
    print max_output

    exit # Prevent processing of stdin after BEGIN block
}

# Function to swap elements in an array (passed by name)
function swap(a, i, j,  temp) {
    temp = a[i]
    a[i] = a[j]
    a[j] = temp
}

# Function to generate permutations recursively
function generate_permutations(arr, k, n,  i) {
    if (k == n) {
        # Process the permutation: run the amplifier chain
        test_phase_setting(arr)
    } else {
        for (i = k; i <= n; i++) {
            swap(arr, k, i)
            generate_permutations(arr, k + 1, n)
            swap(arr, k, i) # backtrack
        }
    }
}

# Function to test a specific phase setting permutation
function test_phase_setting(phase,   i, signal, inputs) {
    signal = 0 # Initial signal

    for (i = 0; i < 5; ++i) {
        delete inputs # Clear inputs for this amp
        inputs[0] = phase[i]
        inputs[1] = signal
        # Run the VM for amplifier i, update signal with its output
        signal = run_vm(inputs)
        if (signal == "HALT_ERROR") { # Check for VM error
             print "Error during VM execution for phase " phase[i] > "/dev/stderr"
             signal = -1 # Indicate error in this chain
             break
        }
    }

    # Update max_output if current signal is higher
    if (signal != -1 && signal > max_output) {
        max_output = signal
    }
}

# Helper: Get parameter value based on mode
function get_param(mem, ip, offset, mode,  addr) {
    # mode 0: position mode, mode 1: immediate mode
    if (mode == 1) {
        return mem[ip + offset]
    } else {
        addr = mem[ip + offset]
        # AWK arrays auto-create with default value (0 or "") if index doesn't exist
        # For Intcode, accessing outside allocated memory might imply reading 0
        return mem[addr]
    }
}

# Intcode VM simulation function
# Takes inputs array (index 0 = phase, index 1 = signal)
# Returns the output value or "HALT_ERROR"
function run_vm(inputs,   mem, ip, input_idx, output_val, i, cmd, opcode, mode1, mode2, p1, p2, addr) {
    # Copy original_code to local mem
    delete mem
    for (i = 0; i < n_orig; ++i) {
         mem[i] = original_code[i]
    }

    ip = 0
    input_idx = 0
    output_val = "NONE" # No output produced yet

    while (ip >= 0 && ip < n_orig) {
        cmd = mem[ip]
        opcode = cmd % 100
        mode1 = int(cmd / 100) % 10
        mode2 = int(cmd / 1000) % 10
        # mode3 = int(cmd / 10000) % 10 # Not needed for param read, only write addr

        if (opcode == 1) { # add
            p1 = get_param(mem, ip, 1, mode1)
            p2 = get_param(mem, ip, 2, mode2)
            addr = mem[ip + 3] # Write address is always position mode conceptually
            mem[addr] = p1 + p2
            ip += 4
        } else if (opcode == 2) { # multiply
            p1 = get_param(mem, ip, 1, mode1)
            p2 = get_param(mem, ip, 2, mode2)
            addr = mem[ip + 3]
            mem[addr] = p1 * p2
            ip += 4
        } else if (opcode == 3) { # input
            addr = mem[ip + 1] # Write address is position mode
             if (input_idx < 2) { # Expecting 2 inputs: phase then signal
                 mem[addr] = inputs[input_idx++]
             } else {
                 print "Error: VM requested more than 2 inputs" > "/dev/stderr"
                 return "HALT_ERROR"
             }
            ip += 2
        } else if (opcode == 4) { # output
            p1 = get_param(mem, ip, 1, mode1)
            output_val = p1 # Store the output
            ip += 2
            # For this problem, we assume one output per amp run then halt
            # If not halting, logic needs change. Let's assume it halts shortly after.
        } else if (opcode == 5) { # jump-if-true
            p1 = get_param(mem, ip, 1, mode1)
            p2 = get_param(mem, ip, 2, mode2)
            if (p1 != 0) {
                ip = p2
            } else {
                ip += 3
            }
        } else if (opcode == 6) { # jump-if-false
            p1 = get_param(mem, ip, 1, mode1)
            p2 = get_param(mem, ip, 2, mode2)
            if (p1 == 0) {
                ip = p2
            } else {
                ip += 3
            }
        } else if (opcode == 7) { # less than
            p1 = get_param(mem, ip, 1, mode1)
            p2 = get_param(mem, ip, 2, mode2)
            addr = mem[ip + 3]
            mem[addr] = (p1 < p2) ? 1 : 0
            ip += 4
        } else if (opcode == 8) { # equals
            p1 = get_param(mem, ip, 1, mode1)
            p2 = get_param(mem, ip, 2, mode2)
            addr = mem[ip + 3]
            mem[addr] = (p1 == p2) ? 1 : 0
            ip += 4
        } else if (opcode == 99) { # halt
            if (output_val == "NONE") { # Halted without producing output?
                print "Warning: VM halted without output" > "/dev/stderr"
                return 0 # Or some default/error value? Let's return 0 for this case.
            }
            return output_val # Return the last output value
        } else {
            print "Error: Unknown opcode " opcode " at ip " ip > "/dev/stderr"
            return "HALT_ERROR"
        }
    }
    print "Error: VM finished unexpectedly (ip out of bounds)" > "/dev/stderr"
    return "HALT_ERROR" # Indicate abnormal termination
}

