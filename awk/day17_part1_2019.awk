
#!/usr/bin/awk -f

function get(p1, m1) {
    if (m1 == 0) { # Position mode
        return MEM[MEM[IP + p1] + 0]
    } else if (m1 == 1) { # Immediate mode
        return MEM[IP + p1] + 0
    } else { # Relative mode
        return MEM[RELBASE + MEM[IP + p1] + 0]
    }
}

function set(p3, m3, val) {
    if (m3 == 0) { # Position mode
        MEM[MEM[IP + p3] + 0] = val
    } else { # Relative mode
        MEM[RELBASE + MEM[IP + p3] + 0] = val
    }
    # Mode 1 is never used for writing parameters
}

function run_program() {
    IP = 0
    RELBASE = 0
    CUR_X = 0
    CUR_Y = 0

    while (1) {
        opcode = MEM[IP] % 100
        m1 = int(MEM[IP] / 100) % 10
        m2 = int(MEM[IP] / 1000) % 10
        m3 = int(MEM[IP] / 10000) % 10

        if (opcode == 1) { # add
            val = get(1, m1) + get(2, m2)
            set(3, m3, val)
            IP += 4
        } else if (opcode == 2) { # mul
            val = get(1, m1) * get(2, m2)
            set(3, m3, val)
            IP += 4
        } else if (opcode == 3) { # input (not needed for part 1, program generates map)
             # set(1, m1, input_value) # Would require input handling if used
             IP += 2 # Assuming no input needed as per Python solution's iter([])
        } else if (opcode == 4) { # output
            val = get(1, m1)
            char = sprintf("%c", val)
             if (char == "\n") {
                CUR_Y++
                CUR_X = 0
            } else {
                if (char == "#" || char == "^" || char == "v" || char == "<" || char == ">") {
                     SCAFFOLD[CUR_X SUBSEP CUR_Y] = 1
                }
                CUR_X++
            }
            IP += 2
        } else if (opcode == 5) { # jump-if-true
            if (get(1, m1) != 0) {
                IP = get(2, m2)
            } else {
                IP += 3
            }
        } else if (opcode == 6) { # jump-if-false
            if (get(1, m1) == 0) {
                IP = get(2, m2)
            } else {
                IP += 3
            }
        } else if (opcode == 7) { # less than
             set(3, m3, (get(1, m1) < get(2, m2)) ? 1 : 0)
            IP += 4
        } else if (opcode == 8) { # equals
             set(3, m3, (get(1, m1) == get(2, m2)) ? 1 : 0)
            IP += 4
        } else if (opcode == 9) { # relative base offset
            RELBASE += get(1, m1)
            IP += 2
        } else if (opcode == 99) { # halt
            break
        } else {
             # Should not happen with valid input
             print "Unknown opcode: " opcode > "/dev/stderr"
             exit 1
        }
    }
}

function calculate_alignment_sum() {
    sum = 0
    for (key in SCAFFOLD) {
        split(key, coords, SUBSEP)
        x = coords[1]
        y = coords[2]

        is_intersection = 1
        if (!((x+1 SUBSEP y) in SCAFFOLD)) is_intersection = 0
        if (is_intersection && !((x-1 SUBSEP y) in SCAFFOLD)) is_intersection = 0
        if (is_intersection && !((x SUBSEP y+1) in SCAFFOLD)) is_intersection = 0
        if (is_intersection && !((x SUBSEP y-1) in SCAFFOLD)) is_intersection = 0

        if (is_intersection) {
            sum += x * y
        }
    }
    return sum
}

# Main entry point equivalent
BEGIN {
    # Read program from input.txt
    if ((getline line < "input.txt") > 0) {
        n = split(line, T, ",")
        for (i = 1; i <= n; i++) {
            MEM[i-1] = T[i] # Use 0-based index for memory
        }
    } else {
        print "Error reading input.txt" > "/dev/stderr"
        exit 1
    }
    close("input.txt")

    # Run the Intcode program which populates SCAFFOLD
    run_program()

    # Calculate and print the sum of alignment parameters
    alignment_sum = calculate_alignment_sum()
    print alignment_sum

    exit # Ensure script exits after BEGIN block
}
