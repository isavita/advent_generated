
#!/usr/bin/awk -f

function decode(n, modes, op) {
    op = n % 100
    n = int(n / 100)
    modes[0] = n % 10
    n = int(n / 10)
    modes[1] = n % 10
    modes[2] = int(n / 10)
    return op
}

function get_addr(idx, mode, addr) {
    addr = data[idx] + 0
    if (mode == 0) return addr
    if (mode == 2) return rel_base + addr
    # Mode 1 (Immediate) address is just the index itself
    # This function is mainly for write targets (modes 0 and 2)
    # Or for getting the address value itself in relative/position mode reads
    return idx
}

function get(idx, mode) {
    val = data[idx] + 0
    if (mode == 1) return val
    addr = get_addr(idx, mode)
    return data[addr] + 0
}

function set(idx, mode, value) {
    addr = get_addr(idx, mode)
    data[addr] = value
}

BEGIN {
    FS = ","
    if ((getline < "input.txt") > 0) {
        for (i = 1; i <= NF; i++) {
            data[i-1] = $i
        }
    } else {
        print "Error: Cannot read input.txt" > "/dev/stderr"; exit 1
    }
    close("input.txt")

    ip = 0
    rel_base = 0
    out_idx = 0

    while (1) {
        instruction = data[ip] + 0
        delete modes
        opcode = decode(instruction, modes)

        if (opcode == 1) { # ADD
            p1 = get(ip + 1, modes[0])
            p2 = get(ip + 2, modes[1])
            set(ip + 3, modes[2], p1 + p2)
            ip += 4
        } else if (opcode == 2) { # MUL
            p1 = get(ip + 1, modes[0])
            p2 = get(ip + 2, modes[1])
            set(ip + 3, modes[2], p1 * p2)
            ip += 4
        } else if (opcode == 3) { # INPUT
            # No input provided in this problem's setup
            print "Error: INPUT opcode executed but no input available" > "/dev/stderr"
            exit 1
            # set(ip + 1, modes[0], input_value) # Placeholder
            # ip += 2
        } else if (opcode == 4) { # OUTPUT
            output[out_idx++] = get(ip + 1, modes[0])
            ip += 2
        } else if (opcode == 5) { # JT
            if (get(ip + 1, modes[0]) != 0) {
                ip = get(ip + 2, modes[1])
            } else {
                ip += 3
            }
        } else if (opcode == 6) { # JF
            if (get(ip + 1, modes[0]) == 0) {
                ip = get(ip + 2, modes[1])
            } else {
                ip += 3
            }
        } else if (opcode == 7) { # LT
            p1 = get(ip + 1, modes[0])
            p2 = get(ip + 2, modes[1])
            set(ip + 3, modes[2], (p1 < p2) ? 1 : 0)
            ip += 4
        } else if (opcode == 8) { # EQ
            p1 = get(ip + 1, modes[0])
            p2 = get(ip + 2, modes[1])
            set(ip + 3, modes[2], (p1 == p2) ? 1 : 0)
            ip += 4
        } else if (opcode == 9) { # RBO
            rel_base += get(ip + 1, modes[0])
            ip += 2
        } else if (opcode == 99) { # HALT
            break
        } else {
            printf "Error: Unknown opcode %d at ip %d\n", opcode, ip > "/dev/stderr"
            exit 1
        }
    }

    block_count = 0
    for (i = 0; i < out_idx; i += 3) {
        if (output[i+2] == 2) {
            block_count++
        }
    }
    print block_count
}

