
#!/usr/bin/awk -f

function get_param(offset, mode,    p) {
    p = mem[ip + offset]
    if (mode == 0) return mem[p]
    if (mode == 1) return p
    if (mode == 2) return mem[rb + p]
    # Should not happen with valid input
    return -99999 # Error indicator
}

function get_write_addr(offset, mode,    p) {
    p = mem[ip + offset]
    if (mode == 0) return p
    if (mode == 2) return rb + p
     # Should not happen with valid input
    return -1 # Error indicator
}

function get_joystick_input() {
    if (ball_x > paddle_x) return 1
    if (ball_x < paddle_x) return -1
    return 0
}

function handle_output(value) {
    output_count++
    if (output_count == 1) {
        output_x = value
    } else if (output_count == 2) {
        output_y = value
    } else { # output_count == 3
        tile_id = value
        if (output_x == -1 && output_y == 0) {
            score = tile_id
        } else {
            if (tile_id == 3) {
                paddle_x = output_x
            } else if (tile_id == 4) {
                ball_x = output_x
            }
        }
        output_count = 0
    }
}

function run_intcode(    instruction, opcode, mode1, mode2, mode3, p1, p2, addr, input_val, output_val) {
    while (1) {
        # Ensure memory access defaults to 0
        if (!(ip in mem)) mem[ip] = 0
        instruction = mem[ip]
        opcode = instruction % 100

        # Calculate modes using integer division implicitly
        mode1 = int(instruction / 100) % 10
        mode2 = int(instruction / 1000) % 10
        mode3 = int(instruction / 10000) % 10

        if (opcode == 99) {
            break
        } else if (opcode == 1) { # Add
            p1 = get_param(1, mode1)
            p2 = get_param(2, mode2)
            addr = get_write_addr(3, mode3)
            mem[addr] = p1 + p2
            ip += 4
        } else if (opcode == 2) { # Multiply
            p1 = get_param(1, mode1)
            p2 = get_param(2, mode2)
            addr = get_write_addr(3, mode3)
            mem[addr] = p1 * p2
            ip += 4
        } else if (opcode == 3) { # Input
            input_val = get_joystick_input()
            addr = get_write_addr(1, mode1)
            mem[addr] = input_val
            ip += 2
        } else if (opcode == 4) { # Output
            output_val = get_param(1, mode1)
            handle_output(output_val)
            ip += 2
        } else if (opcode == 5) { # Jump-if-true
            p1 = get_param(1, mode1)
            p2 = get_param(2, mode2)
            if (p1 != 0) ip = p2
            else ip += 3
        } else if (opcode == 6) { # Jump-if-false
            p1 = get_param(1, mode1)
            p2 = get_param(2, mode2)
            if (p1 == 0) ip = p2
            else ip += 3
        } else if (opcode == 7) { # Less than
            p1 = get_param(1, mode1)
            p2 = get_param(2, mode2)
            addr = get_write_addr(3, mode3)
            mem[addr] = (p1 < p2) ? 1 : 0
            ip += 4
        } else if (opcode == 8) { # Equals
            p1 = get_param(1, mode1)
            p2 = get_param(2, mode2)
            addr = get_write_addr(3, mode3)
            mem[addr] = (p1 == p2) ? 1 : 0
            ip += 4
        } else if (opcode == 9) { # Adjust relative base
            p1 = get_param(1, mode1)
            rb += p1
            ip += 2
        } else {
             # Error state or unknown opcode, halt implicitly by breaking
             break
        }
    }
}

BEGIN {
    FS = ","
    if ((getline < "input.txt") > 0) {
        n = split($0, prog, ",")
        for (i = 1; i <= n; i++) {
            mem[i-1] = prog[i] + 0 # Ensure numeric conversion
        }
    } else {
        exit 1 # Exit if file is empty or cannot be read
    }

    mem[0] = 2 # Free play mode

    ip = 0
    rb = 0
    score = 0
    ball_x = 0
    paddle_x = 0
    output_count = 0
    output_x = 0
    output_y = 0

    run_intcode()
}

END {
    print score
}

