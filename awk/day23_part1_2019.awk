
#!/usr/bin/awk -f

function get_mem(c, addr,    v) {
    if (!(c SUBSEP addr in mem)) {
        return 0
    }
    return mem[c, addr]
}

function set_mem(c, addr, val) {
    mem[c, addr] = val
}

function get_param(c, mode, offset,    addr) {
    if (mode == 0) { # Position mode
        addr = get_mem(c, ip[c] + offset)
        return get_mem(c, addr)
    } else if (mode == 1) { # Immediate mode
        return get_mem(c, ip[c] + offset)
    } else if (mode == 2) { # Relative mode
        addr = rel_base[c] + get_mem(c, ip[c] + offset)
        return get_mem(c, addr)
    }
}

function set_param(c, mode, offset, val,    addr) {
    if (mode == 0) { # Position mode
        addr = get_mem(c, ip[c] + offset)
        set_mem(c, addr, val)
    } else if (mode == 2) { # Relative mode
        addr = rel_base[c] + get_mem(c, ip[c] + offset)
        set_mem(c, addr, val)
    }
}

function add_input(c, val) {
    in_val[c, in_w[c]++] = val
}

function get_input(c,    val) {
    if (in_r[c] < in_w[c]) {
        val = in_val[c, in_r[c]++]
        needs_input[c] = 0
        return val
    } else {
        needs_input[c] = 1
        return -1 # Indicate input needed but queue empty
    }
}

function add_output(c, val) {
    out_buf[c, out_count[c]++] = val
}

# Runs computer 'c' until halt, need input, or full output triplet
function run_computer(c,   opcode, instruction, modes_str, p1, p2, p3, val, target_addr) {
    if (halted[c]) return

    while (1) {
        instruction = get_mem(c, ip[c])
        opcode = instruction % 100
        modes_str = int(instruction / 100)

        p1_mode = modes_str % 10
        p2_mode = int(modes_str / 10) % 10
        p3_mode = int(modes_str / 100) % 10

        if (opcode == 99) {
            halted[c] = 1
            return
        } else if (opcode == 1) { # add
            p1 = get_param(c, p1_mode, 1)
            p2 = get_param(c, p2_mode, 2)
            set_param(c, p3_mode, 3, p1 + p2)
            ip[c] += 4
        } else if (opcode == 2) { # multiply
            p1 = get_param(c, p1_mode, 1)
            p2 = get_param(c, p2_mode, 2)
            set_param(c, p3_mode, 3, p1 * p2)
            ip[c] += 4
        } else if (opcode == 3) { # input
            val = get_input(c)
            if (needs_input[c]) { # Check if get_input signaled need
                 return # Pause execution
            }
            set_param(c, p1_mode, 1, val)
            ip[c] += 2
        } else if (opcode == 4) { # output
            p1 = get_param(c, p1_mode, 1)
            add_output(c, p1)
            ip[c] += 2
            if (out_count[c] % 3 == 0) {
                 return # Pause after full triplet
            }
        } else if (opcode == 5) { # jump-if-true
            p1 = get_param(c, p1_mode, 1)
            p2 = get_param(c, p2_mode, 2)
            if (p1 != 0) {
                ip[c] = p2
            } else {
                ip[c] += 3
            }
        } else if (opcode == 6) { # jump-if-false
            p1 = get_param(c, p1_mode, 1)
            p2 = get_param(c, p2_mode, 2)
            if (p1 == 0) {
                ip[c] = p2
            } else {
                ip[c] += 3
            }
        } else if (opcode == 7) { # less than
            p1 = get_param(c, p1_mode, 1)
            p2 = get_param(c, p2_mode, 2)
            set_param(c, p3_mode, 3, (p1 < p2) ? 1 : 0)
            ip[c] += 4
        } else if (opcode == 8) { # equals
            p1 = get_param(c, p1_mode, 1)
            p2 = get_param(c, p2_mode, 2)
            set_param(c, p3_mode, 3, (p1 == p2) ? 1 : 0)
            ip[c] += 4
        } else if (opcode == 9) { # relative base offset
            p1 = get_param(c, p1_mode, 1)
            rel_base[c] += p1
            ip[c] += 2
        } else {
             printf "Unknown opcode %d at ip %d for computer %d\n", opcode, ip[c], c > "/dev/stderr"
             exit 1
        }
    }
}


BEGIN {
    FS = ","
    if ((getline < "input.txt") <= 0) {
         print "Error reading input.txt" > "/dev/stderr"
         exit 1
    }
    close("input.txt")

    n_initial = split($0, initial_mem_raw, FS)
    for (i = 1; i <= n_initial; i++) {
        initial_mem[i-1] = initial_mem_raw[i]
    }

    NUM_COMPUTERS = 50

    # Initialize computers
    for (c = 0; c < NUM_COMPUTERS; c++) {
        ip[c] = 0
        rel_base[c] = 0
        halted[c] = 0
        needs_input[c] = 0
        in_r[c] = 0; in_w[c] = 0 # Input queue indices
        pq_r[c] = 0; pq_w[c] = 0 # Packet queue indices
        out_count[c] = 0        # Output buffer count

        # Copy initial memory - Use split array indices directly
        for (i = 0; i < n_initial; i++) {
            mem[c, i] = initial_mem[i]
        }
        # Provide initial address input
        add_input(c, c)
    }

    # Main network loop
    while (1) {
        for (c = 0; c < NUM_COMPUTERS; c++) {
            if (halted[c]) continue

            # If computer needs input and its internal queue is empty, check packet queue
            if (needs_input[c] && in_r[c] == in_w[c]) {
                if (pq_r[c] < pq_w[c]) { # Packets available
                    x = pq_val[c, pq_r[c], "x"]
                    y = pq_val[c, pq_r[c], "y"]
                    pq_r[c]++
                    add_input(c, x)
                    add_input(c, y)
                     # needs_input[c] will be reset inside get_input when called by run_computer
                } else { # No packets, provide -1
                    add_input(c, -1)
                     # needs_input[c] will be reset inside get_input when called by run_computer
                }
            }

            # Run the computer
            run_computer(c)

            # Process outputs
            output_idx = 0
            while (output_idx + 2 < out_count[c]) {
                dest = out_buf[c, output_idx]
                x_out = out_buf[c, output_idx + 1]
                y_out = out_buf[c, output_idx + 2]

                if (dest == 255) {
                    print y_out
                    exit 0
                } else if (dest >= 0 && dest < NUM_COMPUTERS) {
                    # Add to destination packet queue
                    pq_val[dest, pq_w[dest], "x"] = x_out
                    pq_val[dest, pq_w[dest], "y"] = y_out
                    pq_w[dest]++
                } # Ignore other addresses

                output_idx += 3
            }

            # Remove processed outputs from buffer (shift remaining)
            if (output_idx > 0) {
                 new_count = 0
                 for (k = output_idx; k < out_count[c]; k++) {
                     out_buf[c, new_count++] = out_buf[c, k]
                 }
                 out_count[c] = new_count
            }
        }
        # Note: No idle detection needed for Part 1 as per the Python logic provided
    }
}

