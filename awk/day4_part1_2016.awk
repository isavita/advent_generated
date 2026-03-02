
BEGIN {
    FS = "[][-]"
    ARGV[1] = "input.txt"
    ARGC = 2
}
NF > 3 {
    split("", cnt)
    split("", list)
    m = 0
    for (i = 1; i <= NF - 3; i++) {
        for (j = 1; j <= length($i); j++) {
            cnt[substr($i, j, 1)]++
        }
    }
    for (char in cnt) {
        list[++m] = sprintf("%05d%s", 10000 - cnt[char], char)
    }
    for (i = 1; i < m; i++) {
        for (j = i + 1; j <= m; j++) {
            if (list[j] < list[i]) {
                tmp = list[i]
                list[i] = list[j]
                list[j] = tmp
            }
        }
    }
    calc = ""
    for (i = 1; i <= 5 && i <= m; i++) {
        calc = calc substr(list[i], 6)
    }
    if (calc == $(NF - 1)) {
        sum += $(NF - 2)
    }
}
END {
    print sum + 0
}
