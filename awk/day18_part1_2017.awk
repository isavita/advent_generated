
#!/usr/bin/awk -f

BEGIN {
    # Read all instructions into memory
    idx = 1
    while (getline < "input.txt" > 0) {
        cmd[idx] = $1
        reg[idx] = $2
        val[idx] = $3 # Store even if empty, check later
        idx++
    }
    close("input.txt")
    num_inst = idx - 1

    # Initialize state
    ip = 1 # Instruction pointer (AWK arrays are 1-based)
    last_snd = 0
    # R = registers (associative array, default value is 0)

    # Main execution loop
    while (ip >= 1 && ip <= num_inst) {
        c = cmd[ip]   # Command
        r = reg[ip]   # Target register name (first argument)
        v_str = val[ip] # Value string or source register name (second argument)

        # Determine the numeric value 'v' from the second argument (v_str)
        # This is needed for set, add, mul, mod, jgz
        v = 0
        if (v_str != "") {
            # Check if v_str is a register name (letters only)
            if (match(v_str, /^[[:alpha:]]+$/)) {
                v = R[v_str] # Get value from register R
            } else {
                v = v_str + 0 # Convert literal number string to number
            }
        }

        # Get the value of the target register 'r'
        # Needed for snd, rcv, jgz check
        # AWK auto-initializes R[r] to 0 if not previously set
        reg_val = R[r]

        # Execute command
        if (c == "snd") {
            last_snd = reg_val
        } else if (c == "set") {
            R[r] = v
        } else if (c == "add") {
            R[r] += v
        } else if (c == "mul") {
            R[r] *= v
        } else if (c == "mod") {
             if (v != 0) { # Avoid division by zero error
                 R[r] = reg_val % v
             }
        } else if (c == "rcv") {
            if (reg_val != 0) {
                print last_snd
                exit # Terminate script successfully
            }
        } else if (c == "jgz") {
            # Check value of the first argument (register r)
            if (reg_val > 0) {
                ip += v # Jump by value v
                continue # Skip the default ip++ at the end of the loop
            }
        }

        ip++ # Go to the next instruction if no jump occurred
    }
    # If loop finishes (ip out of bounds), exit normally (exit code 0)
}
