
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
$2 == "cd" {
    if ($3 == "/") {
        d = 1
        p[d] = "/"
    } else if ($3 == "..") {
        d--
    } else {
        d++
        p[d] = p[d-1] "/" $3
    }
    s[p[d]] += 0
}
$1 ~ /^[0-9]/ {
    for (i = 1; i <= d; i++) {
        s[p[i]] += $1
    }
}
END {
    for (i in s) {
        if (s[i] <= 100000) {
            total += s[i]
        }
    }
    print total
}
