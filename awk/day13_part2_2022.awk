function c(s1, s2, p1, p2, c1, c2) {
    p1 = p2 = 1
    while (p1 <= length(s1) && p2 <= length(s2)) {
        c1 = substr(s1, p1, 1); c2 = substr(s2, p2, 1)
        if (c1 == c2) { p1++; p2++; continue }
        if (c1 == "]") return -1
        if (c2 == "]") return 1
        if (c1 == "[") return c(substr(s1, p1 + 1), substr(s2, p2) "]")
        if (c2 == "[") return c(substr(s1, p1) "]", substr(s2, p2 + 1))
        return c1 < c2 ? -1 : 1
    }
    return 0
}

BEGIN {
    RS = ""
    ARGV[1] = "input.txt"
    ARGC = 2
    i2 = 1
    i6 = 2
}

{
    for (i = 1; i <= NF; i++) {
        s = $i
        gsub(/10/, "A", s)
        gsub(/,/, "", s)
        if (c(s, "[[2]]") < 0) i2++
        if (c(s, "[[6]]") < 0) i6++
    }
}

END {
    print i2 * i6
}