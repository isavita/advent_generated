
#!/usr/bin/awk -f

function get_mem(addr) {
    return mem[addr] + 0
}

function get_param(pnum, modes) {
    mode = int(modes / (10^(pnum-1))) % 10
    val = get_mem(ip + pnum)
    if (mode == 0) {
        return get_mem(val)
    } else if (mode == 1) {
        return val
    } else {
        return get_mem(rb + val)
    }
}

function get_addr(pnum, modes) {
    mode = int(modes / (10^(pnum-1))) % 10
    val = get_mem(ip + pnum)
    if (mode == 0) {
        return val
    } else {
        return rb + val
    }
}

function send_string(s,   len, i) {
    len = length(s)
    for (i = 1; i <= len; i++) {
        input_queue[input_len++] = asc[substr(s, i, 1)]
    }
    input_queue[input_len++] = 10
}

function run_vm(    cmd, opcode, modes, p1, p2, addr) {
    while (1) {
        cmd = get_mem(ip)
        opcode = cmd % 100
        modes = int(cmd / 100)

        if (opcode == 99) break

        if (opcode == 1) {
            p1 = get_param(1, modes)
            p2 = get_param(2, modes)
            addr = get_addr(3, modes)
            mem[addr] = p1 + p2
            ip += 4
        } else if (opcode == 2) {
            p1 = get_param(1, modes)
            p2 = get_param(2, modes)
            addr = get_addr(3, modes)
            mem[addr] = p1 * p2
            ip += 4
        } else if (opcode == 3) {
            addr = get_addr(1, modes)
            mem[addr] = input_queue[input_idx++]
            ip += 2
        } else if (opcode == 4) {
            p1 = get_param(1, modes)
            output_queue[output_idx++] = p1
            ip += 2
        } else if (opcode == 5) {
            p1 = get_param(1, modes)
            p2 = get_param(2, modes)
            if (p1 != 0) ip = p2
            else ip += 3
        } else if (opcode == 6) {
            p1 = get_param(1, modes)
            p2 = get_param(2, modes)
            if (p1 == 0) ip = p2
            else ip += 3
        } else if (opcode == 7) {
            p1 = get_param(1, modes)
            p2 = get_param(2, modes)
            addr = get_addr(3, modes)
            mem[addr] = (p1 < p2) ? 1 : 0
            ip += 4
        } else if (opcode == 8) {
            p1 = get_param(1, modes)
            p2 = get_param(2, modes)
            addr = get_addr(3, modes)
            mem[addr] = (p1 == p2) ? 1 : 0
            ip += 4
        } else if (opcode == 9) {
            p1 = get_param(1, modes)
            rb += p1
            ip += 2
        } else {
             exit 1
        }
    }
}

BEGIN {
    # Build ASCII map
    for (i = 0; i < 128; i++) asc[sprintf("%c", i)] = i

    # Load Intcode program
    if ((getline line < "input.txt") > 0) {
        n = split(line, fields, ",")
        for (i = 1; i <= n; i++) {
            mem[i-1] = fields[i] + 0
        }
    } else {
        exit 1
    }
    close("input.txt")

    ip = 0
    rb = 0
    input_idx = 0
    input_len = 0
    output_idx = 0

    # Define Springdroid instructions
    instructions[0] = "NOT A J"
    instructions[1] = "NOT B T"
    instructions[2] = "OR T J"
    instructions[3] = "NOT C T"
    instructions[4] = "OR T J"
    instructions[5] = "AND D J"
    instructions[6] = "WALK"
    num_instructions = 7

    # Prepare input queue
    for (i = 0; i < num_instructions; i++) {
        send_string(instructions[i])
    }

    # Run the VM
    run_vm()

    # Process output
    for (i = 0; i < output_idx; i++) {
        val = output_queue[i]
        if (val > 127) {
            print val
            exit
        }
    }
    exit
}

