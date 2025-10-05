#!/usr/bin/awk -f
function setChar(str, pos, ch,   pre, post) {
    pre = substr(str, 1, pos-1)
    post = substr(str, pos+1)
    return pre ch post
}
BEGIN {
    cartCount = 0
    lineNum = 0
    while ((getline line < "input.txt") > 0) {
        track[lineNum] = line
        plen = length(line)
        for (p = 1; p <= plen; p++) {
            c = substr(line, p, 1)
            if (c == ">" || c == "<" || c == "^" || c == "v") {
                x[cartCount] = p - 1
                y[cartCount] = lineNum
                dir[cartCount] = c
                turn[cartCount] = 0
                if (c == ">" || c == "<") {
                    track[lineNum] = setChar(track[lineNum], p, "-")
                } else {
                    track[lineNum] = setChar(track[lineNum], p, "|")
                }
                cartCount++
            }
        }
        lineNum++
    }
    totalCarts = cartCount
}
END {
    while (1) {
        for (ci = 0; ci < totalCarts; ci++) {
            d = dir[ci]
            if (d == ">") {
                cell = substr(track[y[ci]], x[ci] + 2, 1)
                if (cell == "\\") { dir[ci] = "v" }
                else if (cell == "/") { dir[ci] = "^" }
                else if (cell == "+") {
                    if (turn[ci] == 0) { dir[ci] = "^"; turn[ci] = 1 }
                    else if (turn[ci] == 1) { turn[ci] = 2 }
                    else if (turn[ci] == 2) { dir[ci] = "v"; turn[ci] = 0 }
                }
                x[ci]++
            } else if (d == "<") {
                cell = substr(track[y[ci]], x[ci], 1)
                if (cell == "/") { dir[ci] = "v" }
                else if (cell == "\\") { dir[ci] = "^" }
                else if (cell == "+") {
                    if (turn[ci] == 0) { dir[ci] = "v"; turn[ci] = 1 }
                    else if (turn[ci] == 1) { turn[ci] = 2 }
                    else if (turn[ci] == 2) { dir[ci] = "^"; turn[ci] = 0 }
                }
                x[ci]--
            } else if (d == "^") {
                cell = substr(track[y[ci] - 1], x[ci] + 1, 1)
                if (cell == "/") { dir[ci] = ">" }
                else if (cell == "\\") { dir[ci] = "<" }
                else if (cell == "+") {
                    if (turn[ci] == 0) { dir[ci] = "<"; turn[ci] = 1 }
                    else if (turn[ci] == 1) { turn[ci] = 2 }
                    else if (turn[ci] == 2) { dir[ci] = ">"; turn[ci] = 0 }
                }
                y[ci]--
            } else if (d == "v") {
                cell = substr(track[y[ci]] + 1, x[ci] + 1, 1)  # not used; replaced below
                cell = substr(track[y[ci] + 1], x[ci] + 1, 1)
                if (cell == "/") { dir[ci] = "<" }
                else if (cell == "\\") { dir[ci] = ">" }
                else if (cell == "+") {
                    if (turn[ci] == 0) { dir[ci] = ">"; turn[ci] = 1 }
                    else if (turn[ci] == 1) { turn[ci] = 2 }
                    else if (turn[ci] == 2) { dir[ci] = "<"; turn[ci] = 0 }
                }
                y[ci]++
            }
        }
        for (i = 0; i < totalCarts; i++) {
            for (j = i + 1; j < totalCarts; j++) {
                if (x[i] == x[j] && y[i] == y[j]) {
                    print x[i] "," y[i]
                    exit
                }
            }
        }
    }
}