
BEGIN {
    FS = "[=, ]"
    ARGV[1] = "input.txt"
    ARGC = 2
}

/^[0-9]/ {
    pts[$1, $2] = 1
}

/fold/ {
    ax = $3; v = $4 + 0
    delete tmp
    for (p in pts) {
        split(p, c, SUBSEP)
        x = c[1] + 0; y = c[2] + 0
        if (ax == "x" && x > v) x = 2 * v - x
        if (ax == "y" && y > v) y = 2 * v - y
        tmp[x, y] = 1
    }
    delete pts
    for (p in tmp) pts[p] = 1
    if (!done++) {
        cnt = 0; for (p in pts) cnt++
        print "Number of dots visible after first fold:", cnt
    }
}

END {
    for (p in pts) {
        split(p, c, SUBSEP)
        if (c[1] + 0 > mx) mx = c[1] + 0
        if (c[2] + 0 > my) my = c[2] + 0
    }
    for (j = 0; j <= my; j++) {
        for (i = 0; i <= mx; i++)
            printf ((i, j) in pts ? "#" : " ")
        printf "\n"
    }
}
