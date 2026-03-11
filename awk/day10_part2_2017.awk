
#!/usr/bin/awk -f

# Day 10: Knot Hash

BEGIN {
    # Set up the input file if not provided as an argument
    if (ARGC < 2) {
        ARGV[1] = "input.txt"
        ARGC = 2
    }

    # Pre-calculate ASCII mapping for character conversion
    for (i = 0; i < 256; i++) {
        ord[sprintf("%c", i)] = i
    }
}

{
    # Knot Hash treats the entire input line as a string of bytes.
    # We trim trailing whitespace/newlines which are often artifacts of file saving.
    gsub(/[ \t\r\n]+$/, "", $0)
    input = $0

    # Step 1: Prepare the sequence of lengths
    # Part 2 requires treating input as ASCII and adding a fixed suffix.
    delete lengths
    len_ptr = 0
    for (i = 1; i <= length(input); i++) {
        lengths[++len_ptr] = ord[substr(input, i, 1)]
    }
    # Standard suffix values
    suffix[1] = 17; suffix[2] = 31; suffix[3] = 73; suffix[4] = 47; suffix[5] = 23
    for (i = 1; i <= 5; i++) {
        lengths[++len_ptr] = suffix[i]
    }

    # Step 2: Initialize the Sparse Hash list (0 to 255)
    for (i = 0; i < 256; i++) list[i] = i

    # Step 3: Perform 64 rounds of shuffling
    pos = 0
    skip = 0
    for (round = 0; round < 64; round++) {
        for (i = 1; i <= len_ptr; i++) {
            L = lengths[i]
            # Reverse the sub-segment of length L starting at 'pos'
            for (k = 0; k < int(L / 2); k++) {
                idx1 = (pos + k) % 256
                idx2 = (pos + L - 1 - k) % 256
                tmp = list[idx1]
                list[idx1] = list[idx2]
                list[idx2] = tmp
            }
            # Update current position and skip size
            pos = (pos + L + skip) % 256
            skip++
        }
    }

    # Step 4: Calculate the Dense Hash
    # Reduce the 256 numbers to 16 numbers by XORing blocks of 16
    dense_hash = ""
    for (block = 0; block < 16; block++) {
        val = list[block * 16]
        for (i = 1; i < 16; i++) {
            val = bitwise_xor(val, list[block * 16 + i])
        }
        # Step 5: Format as hexadecimal string
        dense_hash = dense_hash sprintf("%02x", val)
    }

    print dense_hash
}

# Bitwise XOR function for portability (compatible with original awk)
function bitwise_xor(a, b,   res, bit, k) {
    res = 0
    bit = 1
    # Process 8 bits since list values are 0-255
    for (k = 0; k < 8; k++) {
        if ((int(a / bit) % 2) != (int(b / bit) % 2)) {
            res += bit
        }
        bit *= 2
    }
    return res
}
