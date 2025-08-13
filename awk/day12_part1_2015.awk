
#!/usr/bin/awk -f

BEGIN {
    sum = 0
    while ((getline line < "input.txt") > 0) {
        while (match(line, /-?[0-9]+/)) {
            sum += substr(line, RSTART, RLENGTH) + 0
            line = substr(line, RSTART + RLENGTH)
        }
    }
    print sum
}
