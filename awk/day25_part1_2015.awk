
BEGIN {
    while ((getline < "input.txt") > 0) {
        if (match($0, /row [0-9]+, column [0-9]+/)) {
            split(substr($0, RSTART, RLENGTH), a, /[^0-9]+/)
            r = a[2]
            c = a[3]
        }
    }
    pos = (r + c - 2) * (r + c - 1) / 2 + c
    code = 20151125
    for (i = 1; i < pos; i++) {
        code = (code * 252533) % 33554393
    }
    print code
}
