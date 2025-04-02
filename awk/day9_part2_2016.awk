
# Function for v1 decompression length (non-recursive)
function decompressed_length_v1(data,    l, i, m_end, m_str, p, n, r, c) {
    l = 0
    i = 1
    while (i <= length(data)) {
        c = substr(data, i, 1)
        if (c == "(") {
            # Find the closing parenthesis relative to current position
            if (match(substr(data, i + 1), /[)]/)) {
                m_end = i + RSTART # Absolute index of ')'
                m_str = substr(data, i + 1, RSTART - 1) # Content within ()
                split(m_str, p, "x")
                n = p[1] + 0 # Ensure numeric
                r = p[2] + 0 # Ensure numeric
                l += n * r
                i = m_end + n + 1 # Move past marker and sequence
            } else {
                # Malformed: treat '(' as literal char if ')' not found after it
                l++
                i++
            }
        } else {
            l++
            i++
        }
    }
    return l
}

# Function for v2 decompression length (recursive)
function decompressed_length_v2(data,    l, i, m_end, m_str, p, n, r, sub_data, sub_len, c) {
    l = 0
    i = 1
    while (i <= length(data)) {
        c = substr(data, i, 1)
        if (c == "(") {
            if (match(substr(data, i + 1), /[)]/)) {
                m_end = i + RSTART
                m_str = substr(data, i + 1, RSTART - 1)
                split(m_str, p, "x")
                n = p[1] + 0
                r = p[2] + 0
                # Get the substring to decompress recursively
                sub_data = substr(data, m_end + 1, n)
                # Recursive call
                sub_len = decompressed_length_v2(sub_data)
                l += sub_len * r
                i = m_end + n + 1 # Move past marker and sequence
            } else {
                l++
                i++
            }
        } else {
            l++
            i++
        }
    }
    return l
}

# Main entry point: Read file in BEGIN block
BEGIN {
    # Read the first line from input.txt (assuming single line like Python read().strip())
    if ((getline data < "input.txt") > 0) {
         # Strip leading/trailing whitespace, similar to Python's strip()
         gsub(/^[[:space:]]+|[[:space:]]+$/, "", data)
    } else {
        print "Error: Cannot read input.txt" > "/dev/stderr"
        exit 1
    }
    close("input.txt")

    # Calculate and print results
    print decompressed_length_v1(data)
    print decompressed_length_v2(data)
}
