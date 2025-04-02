
#!/usr/bin/awk -f

function acopy(src, dest,      idx) {
    delete dest
    for (idx in src) {
        dest[idx] = src[idx]
    }
}

function get_p(offset, mode,    param_val, param_addr) {
    param_val = mem[ip + offset] + 0
    if (mode == 0) { param_addr = param_val; return mem[param_addr] + 0 }
    if (mode == 1) { return param_val }
    if (mode == 2) { param_addr = rb + param_val; return mem[param_addr] + 0 }
}

function get_target_addr(offset, mode,    param_val) {
     param_val = mem[ip + offset] + 0
     if (mode == 0) { return param_val }
     if (mode == 2) { return rb + param_val }
     printf "Error: Invalid mode %d for target address at ip %d\n", mode, ip > "/dev/stderr"; exit 1
}

function run_vm_loop(    cmd, opcode, modes, m1, m2, m3, v1, v2, target_addr) {
    while (mem[ip] != 99) {
        cmd = mem[ip] + 0
        opcode = cmd % 100
        modes = int(cmd / 100)
        m1 = modes % 10
        m2 = int(modes / 10) % 10
        m3 = int(modes / 100) % 10

        if (opcode == 1) {
            v1 = get_p(1, m1)
            v2 = get_p(2, m2)
            target_addr = get_target_addr(3, m3)
            mem[target_addr] = v1 + v2
            ip += 4
        } else if (opcode == 2) {
             v1 = get_p(1, m1)
             v2 = get_p(2, m2)
             target_addr = get_target_addr(3, m3)
             mem[target_addr] = v1 * v2
             ip += 4
        } else if (opcode == 3) {
            if (input_idx >= length(input_queue)) { printf "Error: Input requested but none available\n" > "/dev/stderr"; exit 1 }
            target_addr = get_target_addr(1, m1)
            mem[target_addr] = input_queue[input_idx++]
            ip += 2
        } else if (opcode == 4) {
            v1 = get_p(1, m1)
            g_last_output = v1
            ip += 2
        } else if (opcode == 5) {
            v1 = get_p(1, m1)
            v2 = get_p(2, m2)
            if (v1 != 0) ip = v2; else ip += 3
        } else if (opcode == 6) {
            v1 = get_p(1, m1)
            v2 = get_p(2, m2)
            if (v1 == 0) ip = v2; else ip += 3
        } else if (opcode == 7) {
            v1 = get_p(1, m1)
            v2 = get_p(2, m2)
            target_addr = get_target_addr(3, m3)
            mem[target_addr] = (v1 < v2) ? 1 : 0
            ip += 4
        } else if (opcode == 8) {
            v1 = get_p(1, m1)
            v2 = get_p(2, m2)
            target_addr = get_target_addr(3, m3)
            mem[target_addr] = (v1 == v2) ? 1 : 0
            ip += 4
        } else if (opcode == 9) {
            v1 = get_p(1, m1)
            rb += v1
            ip += 2
        } else {
             printf "Error: Unknown opcode %d at ip %d\n", opcode, ip > "/dev/stderr"; exit 1
        }
    }
}

function beam(x, y) {
    acopy(initial_mem, mem)
    ip = 0
    rb = 0
    delete input_queue
    input_queue[0] = x
    input_queue[1] = y
    input_idx = 0
    g_last_output = 0

    run_vm_loop()

    return g_last_output == 1
}

BEGIN {
    FS=","
    if ((getline line < "input.txt") > 0) {
        n = split(line, parts, FS)
        for (i = 1; i <= n; i++) {
            initial_mem[i-1] = parts[i]
        }
    }
    close("input.txt")

    y = 20 # Start slightly higher as per Python example logic
    x = 0
    while (1) {
        while (!beam(x, y)) {
            x++
        }

        if (!beam(x + 99, y)) {
            y++
            continue
        }

        if (!beam(x, y + 99)) {
            x++
            continue
        }

        print x * 10000 + y
        exit
    }
}
