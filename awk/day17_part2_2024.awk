
#!/usr/bin/awk -f

# Bitwise XOR function for standard AWK
function bit_xor(a, b,   res, m, bit_a, bit_b) {
    res = 0; m = 1; a = int(a); b = int(b);
    while (a > 0 || b > 0) {
        bit_a = a % 2; bit_b = b % 2;
        if (bit_a != bit_b) res += m;
        a = int(a / 2); b = int(b / 2); m *= 2;
    }
    return res;
}

# Simulates the 3-bit computer
function run_program(a_init,    a, b, c, ip, out_str, opcode, operand, combo, out_val) {
    a = a_init; b = rb_init; c = rc_init; ip = 0; out_str = "";
    while (ip < plen) {
        opcode = prog[ip]; operand = prog[ip+1];
        # Combo operand determination
        if (operand <= 3) combo = operand;
        else if (operand == 4) combo = a;
        else if (operand == 5) combo = b;
        else if (operand == 6) combo = c;
        
        if (opcode == 0) {        # adv: A = floor(A / 2^combo)
            a = int(a / (2^combo));
        } else if (opcode == 1) { # bxl: B = B XOR literal
            b = bit_xor(b, operand);
        } else if (opcode == 2) { # bst: B = combo % 8
            b = combo % 8;
        } else if (opcode == 3) { # jnz: if A != 0 jump
            if (a != 0) { ip = operand; continue; }
        } else if (opcode == 4) { # bxc: B = B XOR C
            b = bit_xor(b, c);
        } else if (opcode == 5) { # out: output combo % 8
            out_val = combo % 8;
            out_str = (out_str == "" ? "" : out_str ",") int(out_val < 0 ? out_val + 8 : out_val);
        } else if (opcode == 6) { # bdv: B = floor(A / 2^combo)
            b = int(a / (2^combo));
        } else if (opcode == 7) { # cdv: C = floor(A / 2^combo)
            c = int(a / (2^combo));
        }
        ip += 2;
    }
    return out_str;
}

# Recursive solver for Part 2: finding the lowest A that produces a copy of the program
function solve_part2(val, step,    d, curr_a, res_str, target, final_res, k) {
    if (step < 0) return val;

    # Construct the target output suffix we need to match
    target = "";
    for (k = step; k < plen; k++) {
        target = (target == "" ? "" : target ",") prog[k];
    }

    # Try every 3-bit combination (0-7) for the current position in A
    for (d = 0; d < 8; d++) {
        curr_a = val * 8 + d;
        # Skip 0 for the highest bit to ensure the value is positive
        if (curr_a == 0 && step == plen - 1) continue;
        
        # Check if this value of A produces the desired output suffix
        res_str = run_program(curr_a);
        if (res_str == target) {
            final_res = solve_part2(curr_a, step - 1);
            if (final_res != -1) return final_res;
        }
    }
    return -1;
}

BEGIN {
    # Ensure it reads from input.txt by default
    if (ARGC < 2) ARGV[ARGC++] = "input.txt";
    FS = ": ";
}

# Parse input registers and program
{
    if ($1 ~ /Register A/) ra_init = $2;
    else if ($1 ~ /Register B/) rb_init = $2;
    else if ($1 ~ /Register C/) rc_init = $2;
    else if ($1 ~ /Program/) {
        n = split($2, prog_all, ",");
        for (i = 1; i <= n; i++) prog[i-1] = prog_all[i];
        plen = n;
    }
}

END {
    # Output result for Part 1
    print run_program(ra_init);

    # Output result for Part 2
    # DFS starts from the end of the program to build Register A 3 bits at a time
    ans_p2 = solve_part2(0, plen - 1);
    if (ans_p2 != -1) {
        printf "%0.f\n", ans_p2;
    }
}

