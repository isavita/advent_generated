
#!/usr/bin/awk -f
BEGIN {
    enabled = 1
    total   = 0
    pos     = 1
    input   = ""
    while (getline line < "input.txt" > 0) input = input line
}
END {
    len = length(input)
    while (pos <= len) {
        # mul(a,b)
        if (substr(input, pos, 4) == "mul(") {
            pos += 4
            n1 = 0
            while (pos <= len && substr(input, pos, 1) ~ /[0-9]/) {
                n1 = n1 * 10 + (substr(input, pos, 1) + 0)
                pos++
            }
            if (pos > len || substr(input, pos, 1) != ",") { pos++; continue }
            pos++
            n2 = 0
            while (pos <= len && substr(input, pos, 1) ~ /[0-9]/) {
                n2 = n2 * 10 + (substr(input, pos, 1) + 0)
                pos++
            }
            if (pos > len || substr(input, pos, 1) != ")") { pos++; continue }
            if (enabled) total += n1 * n2
            pos++
        }
        # do()
        else if (substr(input, pos, 4) == "do()") {
            enabled = 1
            pos += 4
        }
        # don't()
        else if (substr(input, pos, 7) == "don't()") {
            enabled = 0
            pos += 7
        }
        else {
            pos++
        }
    }
    print total
}
