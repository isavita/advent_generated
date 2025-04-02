
#!/usr/bin/awk -f

# AWK implementation of the Intcode Network simulation

function get_mem(id, addr) {
    return (id SUBSEP addr in mem) ? mem[id, addr] : 0
}

function set_mem(id, addr, val) {
    mem[id, addr] = val
}

function get_param(id, mode, offset) {
    p_val = get_mem(id, ip[id] + offset)
    if (mode == 0) { # Position mode
        return get_mem(id, p_val)
    } else if (mode == 1) { # Immediate mode
        return p_val
    } else if (mode == 2) { # Relative mode
        return get_mem(id, rb[id] + p_val)
    } else {
        print "Error: Unknown parameter mode " mode > "/dev/stderr"
        exit 1
    }
}

function set_param(id, mode, offset, value) {
    p_val = get_mem(id, ip[id] + offset)
    if (mode == 0) { # Position mode
        set_mem(id, p_val, value)
    } else if (mode == 2) { # Relative mode
        set_mem(id, rb[id] + p_val, value)
    } else {
        print "Error: Unknown parameter mode for writing " mode > "/dev/stderr"
        exit 1
    }
}

function run_computer(id) {
    if (halted[id]) return

    while (1) {
        opcode_full = get_mem(id, ip[id])
        opcode = opcode_full % 100
        modes[1] = int(opcode_full / 100) % 10
        modes[2] = int(opcode_full / 1000) % 10
        modes[3] = int(opcode_full / 10000) % 10

        if (opcode == 99) {
            halted[id] = 1
            return
        } else if (opcode == 1) { # add
            p1 = get_param(id, modes[1], 1)
            p2 = get_param(id, modes[2], 2)
            set_param(id, modes[3], 3, p1 + p2)
            ip[id] += 4
        } else if (opcode == 2) { # multiply
            p1 = get_param(id, modes[1], 1)
            p2 = get_param(id, modes[2], 2)
            set_param(id, modes[3], 3, p1 * p2)
            ip[id] += 4
        } else if (opcode == 3) { # input
            if (input_tail[id] <= input_head[id]) {
                 # No input available, provide -1 and pause
                set_param(id, modes[1], 1, -1)
                ip[id] += 2
                idle[id] = 1 # Set idle flag
                return # Pause execution until next cycle
            } else {
                value = inputs[id, input_head[id]++]
                set_param(id, modes[1], 1, value)
                ip[id] += 2
                idle[id] = 0 # Consumed input, not idle
            }
        } else if (opcode == 4) { # output
            p1 = get_param(id, modes[1], 1)
            outputs[id, output_tail[id]++] = p1
            ip[id] += 2
            idle[id] = 0 # Produced output, not idle
            if (output_tail[id] - output_head[id] == 3) {
                return # Return control after producing a full packet
            }
        } else if (opcode == 5) { # jump-if-true
            p1 = get_param(id, modes[1], 1)
            p2 = get_param(id, modes[2], 2)
            if (p1 != 0) {
                ip[id] = p2
            } else {
                ip[id] += 3
            }
        } else if (opcode == 6) { # jump-if-false
            p1 = get_param(id, modes[1], 1)
            p2 = get_param(id, modes[2], 2)
            if (p1 == 0) {
                ip[id] = p2
            } else {
                ip[id] += 3
            }
        } else if (opcode == 7) { # less than
            p1 = get_param(id, modes[1], 1)
            p2 = get_param(id, modes[2], 2)
            set_param(id, modes[3], 3, (p1 < p2) ? 1 : 0)
            ip[id] += 4
        } else if (opcode == 8) { # equals
            p1 = get_param(id, modes[1], 1)
            p2 = get_param(id, modes[2], 2)
            set_param(id, modes[3], 3, (p1 == p2) ? 1 : 0)
            ip[id] += 4
        } else if (opcode == 9) { # relative base offset
            p1 = get_param(id, modes[1], 1)
            rb[id] += p1
            ip[id] += 2
        } else {
            print "Error: Unknown opcode " opcode " at ip " ip[id] " for computer " id > "/dev/stderr"
            exit 1
        }
    }
}

BEGIN {
    FS = ","
    if ((getline line < "input.txt") > 0) {
        n_prog = split(line, prog_fields, FS)
        for (i = 1; i <= n_prog; ++i) {
            program[i-1] = prog_fields[i]
        }
    } else {
        print "Error reading input.txt" > "/dev/stderr"
        exit 1
    }
    close("input.txt")

    # Initialize 50 computers
    num_computers = 50
    for (id = 0; id < num_computers; ++id) {
        # Copy program memory
        for (i = 0; i < n_prog; ++i) {
            mem[id, i] = program[i]
        }
        ip[id] = 0
        rb[id] = 0
        halted[id] = 0
        idle[id] = 0 # Initially not idle (has initial input)

        # Initialize queues
        input_head[id] = 0
        input_tail[id] = 0
        output_head[id] = 0
        output_tail[id] = 0
        pq_head[id] = 0
        pq_tail[id] = 0

        # Provide initial address input
        inputs[id, input_tail[id]++] = id
    }

    nat_packet_set = 0
    prev_nat_y_set = 0

    # Main simulation loop
    while (1) {
        network_is_idle = 1 # Assume idle initially

        for (i = 0; i < num_computers; ++i) {
             if (halted[i]) continue

            # Provide packet input if available, else mark for -1 input
            input_provided = 0
            if (pq_tail[i] > pq_head[i]) {
                x = pq[i, pq_head[i]++]
                y = pq[i, pq_head[i]++]
                inputs[i, input_tail[i]++] = x
                inputs[i, input_tail[i]++] = y
                idle[i] = 0 # Has input, not idle
                input_provided = 1
                network_is_idle = 0 # Activity due to packet queue
            }

            # Run the computer (it will request input internally if needed)
            run_computer(i)

            # Process outputs
            while (output_tail[i] - output_head[i] >= 3) {
                network_is_idle = 0 # Activity due to output
                dest = outputs[i, output_head[i]++]
                x = outputs[i, output_head[i]++]
                y = outputs[i, output_head[i]++]

                if (dest == 255) {
                    nat_x = x
                    nat_y = y
                    nat_packet_set = 1
                } else if (dest >= 0 && dest < num_computers) {
                    pq[dest, pq_tail[dest]++] = x
                    pq[dest, pq_tail[dest]++] = y
                }
            }

             # Update network idle status based on computer state *after* running
             if (!idle[i] || (pq_tail[i] > pq_head[i])) {
                 network_is_idle = 0
             }
        }

        # Check for network idle condition
        if (network_is_idle) {
            if (nat_packet_set) {
                 # Send NAT packet to computer 0
                pq[0, pq_tail[0]++] = nat_x
                pq[0, pq_tail[0]++] = nat_y

                 # Check for repeated Y value
                if (prev_nat_y_set && nat_y == prev_nat_y) {
                    print nat_y
                    exit 0
                }
                prev_nat_y = nat_y
                prev_nat_y_set = 1
                 # Resetting nat_packet_set = 0 here would be wrong, keep last packet
            } else {
                # Network is idle but NAT has no packet - potential deadlock?
                # The problem description implies NAT will eventually get a packet.
                # Or all computers might halt. If all halted and network idle, exit.
                all_halted = 1
                for(j=0; j<num_computers; ++j) {
                    if (!halted[j]) {
                        all_halted = 0
                        break
                    }
                }
                if (all_halted) {
                    print "All computers halted, network idle, no NAT packet. Exiting." > "/dev/stderr"
                    exit 1;
                }
            }
        }
    }
}

# No main block needed as BEGIN/END handles everything
