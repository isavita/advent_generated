BEGIN {
    while (getline < "input.txt" > 0) {
        q = r = 0
        for (i = 1; i <= length($0); i++) {
            c = substr($0, i, 1)
            if (c == "e") {q++; continue}
            if (c == "w") {q--; continue}
            if (c == "n" || c == "s") {
                c2 = substr($0, ++i, 1)
                if (c == "n") {
                    if (c2 == "e") {q++; r--}
                    if (c2 == "w") r--
                } else {
                    if (c2 == "e") r++
                    if (c2 == "w") {q--; r++}
                }
            }
        }
        key = q SUBSEP r
        cnt[key] = !cnt[key]
    }
    for (k in cnt) if (cnt[k]) sum++
    print sum
}