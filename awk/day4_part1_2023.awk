
BEGIN {
    total = 0
    while ((getline line < "input.txt") > 0) {
        split(line, a, ":")
        split(a[2], b, "|")
        split(b[1], w, " ")
        split(b[2], y, " ")
        pts = 0
        for (i in y) {
            for (j in w) {
                if (y[i] == w[j] && y[i] != "") {
                    pts = pts ? pts * 2 : 1
                    break
                }
            }
        }
        total += pts
    }
    print total
}
