
#!/usr/bin/awk -f

BEGIN {
    if ((getline line < "input.txt") <= 0) {
        exit 1
    }
    close("input.txt")

    n = split(line, tmp_data, ",")
    for (i = 1; i <= n; i++) {
        d[i-1] = tmp_data[i] + 0
    }

    d[1] = 12
    d[2] = 2

    pos = 0
    while (d[pos] != 99) {
        opcode = d[pos]
        idx1 = d[pos + 1]
        idx2 = d[pos + 2]
        dest = d[pos + 3]

        if (opcode == 1) {
            d[dest] = d[idx1] + d[idx2]
        } else if (opcode == 2) {
            d[dest] = d[idx1] * d[idx2]
        }
        pos += 4
    }

    print d[0]
    exit 0
}
