
#!/usr/bin/awk -f

# --- Day 14: Disk Defragmentation ---
# This program reads a key from input.txt, generates a 128x128 grid using 
# Knot Hashes, and calculates both the number of used squares and the 
# number of contiguous regions.

# Bitwise XOR for 8-bit numbers (for cross-version compatibility)
function bit_xor(a, b,    res, p, i) {
    res = 0; p = 1
    for (i = 0; i < 8; i++) {
        if ((a % 2) != (b % 2)) res += p
        a = int(a / 2); b = int(b / 2); p *= 2
    }
    return res
}

# Knot Hash calculation (Standard Day 10 Part 2 implementation)
function knot_hash(input,    list, lengths, i, j, k, curr, skip, len_input, tmp, dense, out, round, len, idx1, idx2) {
    len_input = length(input)
    # Convert input to ASCII codes
    for (i = 1; i <= len_input; i++) {
        lengths[i] = ORD[substr(input, i, 1)]
    }
    # Standard length salt suffix
    lengths[++len_input] = 17
    lengths[++len_input] = 31
    lengths[++len_input] = 73
    lengths[++len_input] = 47
    lengths[++len_input] = 23

    # Initialize sparse hash list [0..255]
    for (i = 0; i < 256; i++) list[i] = i

    curr = 0; skip = 0
    # Run 64 rounds of shuffling
    for (round = 0; round < 64; round++) {
        for (i = 1; i <= len_input; i++) {
            len = lengths[i]
            # Reverse sublist of length len
            for (k = 0; k < int(len / 2); k++) {
                idx1 = (curr + k) % 256
                idx2 = (curr + len - 1 - k) % 256
                tmp = list[idx1]; list[idx1] = list[idx2]; list[idx2] = tmp
            }
            curr = (curr + len + skip) % 256
            skip++
        }
    }

    # Compress into 16-byte dense hash via XOR
    out = ""
    for (i = 0; i < 16; i++) {
        dense = list[i * 16]
        for (j = 1; j < 16; j++) {
            dense = bit_xor(dense, list[i * 16 + j])
        }
        out = out sprintf("%02x", dense)
    }
    return out
}

# Stack-based flood fill to avoid recursion limits
function flood_fill(r, c,    stack_ptr, stack_r, stack_c, cr, cc) {
    stack_ptr = 1
    stack_r[stack_ptr] = r
    stack_c[stack_ptr] = c
    GRID[r, c] = 0 # Mark as visited

    while (stack_ptr > 0) {
        cr = stack_r[stack_ptr]
        cc = stack_c[stack_ptr--]

        # Check 4 orthogonal neighbors
        if (cr > 0 && GRID[cr-1, cc] == 1) { 
            GRID[cr-1, cc] = 0; stack_r[++stack_ptr] = cr-1; stack_c[stack_ptr] = cc 
        }
        if (cr < 127 && GRID[cr+1, cc] == 1) { 
            GRID[cr+1, cc] = 0; stack_r[++stack_ptr] = cr+1; stack_c[stack_ptr] = cc 
        }
        if (cc > 0 && GRID[cr, cc-1] == 1) { 
            GRID[cr, cc-1] = 0; stack_r[++stack_ptr] = cr; stack_c[stack_ptr] = cc-1 
        }
        if (cc < 127 && GRID[cr, cc+1] == 1) { 
            GRID[cr, cc+1] = 0; stack_r[++stack_ptr] = cr; stack_c[stack_ptr] = cc+1 
        }
    }
}

BEGIN {
    # Pre-compute ORD mapping and Hex-to-Binary lookup
    for (i = 0; i < 256; i++) ORD[sprintf("%c", i)] = i
    split("0000 0001 0010 0011 0100 0101 0110 0111 1000 1001 1010 1011 1100 1101 1110 1111", h2b)
    split("0 1 2 3 4 5 6 7 8 9 a b c d e f", h_chars)
    for(i=1; i<=16; i++) HEX2BIT[h_chars[i]] = h2b[i]

    # Read input key
    input_file = "input.txt"
    if ((getline key < input_file) <= 0) exit
    gsub(/[ \t\r\n]+$/, "", key)

    # Process grid: Part 1 logic
    total_used = 0
    for (r = 0; r < 128; r++) {
        h = knot_hash(key "-" r)
        bits_str = ""
        for (i = 1; i <= 32; i++) {
            bits_str = bits_str HEX2BIT[substr(h, i, 1)]
        }
        for (c = 1; c <= 128; c++) {
            val = substr(bits_str, c, 1)
            GRID[r, c-1] = val
            if (val == "1") total_used++
        }
    }
    print "Part 1:", total_used

    # Calculate regions: Part 2 logic
    regions = 0
    for (r = 0; r < 128; r++) {
        for (c = 0; c < 128; c++) {
            if (GRID[r, c] == 1) {
                regions++
                flood_fill(r, c)
            }
        }
    }
    print "Part 2:", regions
}

