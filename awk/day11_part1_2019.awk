
#!/usr/bin/awk -f

# AWK implementation of the Intcode computer and Robot Painter

# Global variables for Intcode state
# mem: Associative array holding the program/memory (0-indexed)
# ip: Instruction Pointer (0-indexed)
# halted: Flag indicating if the program has halted (0 or 1)
# input_q: Array simulating the input queue
# input_head: Index of the next input to read
# input_tail: Index where the next input will be added
# output_q: Array simulating the output buffer
# output_tail: Number of elements in the output buffer

# Global variables for Robot state
# rx, ry: Robot's current coordinates
# rd: Robot's current direction (0: Up, 1: Right, 2: Down, 3: Left)
# grid: Associative array storing panel colors (key: "x,y", value: 0 or 1)

# --- Intcode Helper Functions ---

function get_mode(offset) {
    # Determine parameter mode (0=position, 1=immediate)
    return int(mem[ip] / (10^(1 + offset))) % 10
}

function get_param(offset) {
    # Get parameter value based on mode
    mode = get_mode(offset)
    addr = ip + offset
    if (mode == 1) { # Immediate mode
        return mem[addr]
    } else { # Position mode
        # AWK returns 0 for non-existent keys, matching Intcode requirements
        return mem[mem[addr]]
    }
}

function get_addr(offset) {
    # Get address for writing (always position mode interpretation)
    addr = ip + offset
    # AWK returns 0 for non-existent keys, which works here
    return mem[addr]
}

function add_input(value) {
    input_q[input_tail++] = value
}

function has_input() {
    return input_head < input_tail
}

function read_input() {
    if (has_input()) {
        return input_q[input_head++]
    } else {
        # This case indicates the run should pause, handled in run()
        print "Error: Attempted to read empty input queue" > "/dev/stderr"
        # We rely on the check in run() before calling this
        return -1 # Error indicator, should not happen with proper checks
    }
}

function add_output(value) {
    output_q[output_tail++] = value
}

function clear_output() {
    output_tail = 0
    # No need to actually delete array elements in AWK for this usage
}

# --- Intcode Run Function ---

function run() {
    clear_output()
    while (1) {
        # Ensure memory exists (AWK handles this implicitly, returns 0 if unset)
        opcode = mem[ip] % 100

        if (opcode == 1) { # Add
            val1 = get_param(1)
            val2 = get_param(2)
            addr = get_addr(3)
            mem[addr] = val1 + val2
            ip += 4
        } else if (opcode == 2) { # Multiply
            val1 = get_param(1)
            val2 = get_param(2)
            addr = get_addr(3)
            mem[addr] = val1 * val2
            ip += 4
        } else if (opcode == 3) { # Input
            if (!has_input()) {
                return # Pause execution, wait for more input
            }
            addr = get_addr(1)
            mem[addr] = read_input()
            ip += 2
        } else if (opcode == 4) { # Output
            val = get_param(1)
            add_output(val)
            ip += 2
        } else if (opcode == 5) { # Jump-if-true
            val = get_param(1)
            target = get_param(2)
            if (val != 0) {
                ip = target
            } else {
                ip += 3
            }
        } else if (opcode == 6) { # Jump-if-false
            val = get_param(1)
            target = get_param(2)
            if (val == 0) {
                ip = target
            } else {
                ip += 3
            }
        } else if (opcode == 7) { # Less than
            val1 = get_param(1)
            val2 = get_param(2)
            addr = get_addr(3)
            mem[addr] = (val1 < val2) ? 1 : 0
            ip += 4
        } else if (opcode == 8) { # Equals
            val1 = get_param(1)
            val2 = get_param(2)
            addr = get_addr(3)
            mem[addr] = (val1 == val2) ? 1 : 0
            ip += 4
        } else if (opcode == 99) { # Halt
            halted = 1
            return
        } else {
            printf "Unknown opcode: %d at ip %d\n", mem[ip], ip > "/dev/stderr"
            halted = 1 # Treat unknown opcode as halt
            return
        }
    }
}

# --- Robot Functions ---

function turn_and_move(turn_direction) {
    # turn_direction: 0 for left, 1 for right
    if (turn_direction == 0) {
        rd = (rd - 1 + 4) % 4 # Turn left (handle negative modulo)
    } else {
        rd = (rd + 1) % 4     # Turn right
    }

    # Move forward
    if (rd == 0) { # Up
        ry--
    } else if (rd == 1) { # Right
        rx++
    } else if (rd == 2) { # Down
        ry++
    } else { # Left (rd == 3)
        rx--
    }
}

# --- Main Execution ---

BEGIN {
    # Initialize FS for reading comma-separated input
    FS = ","

    # Read program from input.txt
    if ((getline line < "input.txt") > 0) {
        n = split(line, parts, FS)
        for (i = 1; i <= n; i++) {
            mem[i-1] = parts[i] + 0 # Ensure numeric conversion, 0-based index
        }
    } else {
        print "Error: Could not read input.txt" > "/dev/stderr"
        exit 1
    }
    close("input.txt")

    # Initialize state
    ip = 0
    halted = 0
    input_head = 0
    input_tail = 0
    output_tail = 0
    rx = 0
    ry = 0
    rd = 0 # Start facing Up

    # Main simulation loop
    while (!halted) {
        # Get current panel color (default 0/black)
        pos = rx "," ry
        current_color = (pos in grid) ? grid[pos] : 0

        # Provide color as input
        add_input(current_color)

        # Run Intcode until halt or input needed
        run()

        # Process output if program hasn't halted
        if (!halted) {
             if (output_tail == 2) {
                # Output[0] is color to paint, Output[1] is turn direction
                grid[pos] = output_q[0]
                turn_and_move(output_q[1])
             } else if (output_tail != 0) {
                 # Should not happen in this problem if run paused correctly
                 print "Error: Unexpected number of outputs:", output_tail > "/dev/stderr"
                 halted = 1 # Stop simulation on error
             }
             # If output_tail is 0, it means run() paused waiting for input, continue loop
        }
    }

    # Count the number of painted panels
    painted_count = 0
    for (key in grid) {
        painted_count++
    }
    print painted_count
}

