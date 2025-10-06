function main() {
    idx = 0
    x[0] = 1
    while ((getline line < "input.txt") > 0) {
        if (line ~ /^noop/) {
            idx++
            x[idx] = x[idx - 1]
        } else if (line ~ /^addx/) {
            split(line, a, " ")
            n = a[2] + 0
            idx++
            x[idx] = x[idx - 1]
            idx++
            x[idx] = x[idx - 1] + n
        }
    }
    close("input.txt")
    sum = 0
    for (i = 0; i <= idx; i++) {
        if ((i % 40) == 19) {
            sum += (i + 1) * x[i]
        }
    }
    print sum
}
BEGIN { main() }