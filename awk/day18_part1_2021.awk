BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}

function explode(i, j) {
    for (i = 1; i < n; i++) {
        if (dep[i] > 4 && dep[i] == dep[i + 1]) {
            if (i > 1) val[i - 1] += val[i]
            if (i + 1 < n) val[i + 2] += val[i + 1]
            val[i] = 0
            dep[i]--
            for (j = i + 1; j < n; j++) {
                val[j] = val[j + 1]
                dep[j] = dep[j + 1]
            }
            n--
            return 1
        }
    }
    return 0
}

function split_val(i, j, v, d) {
    for (i = 1; i <= n; i++) {
        if (val[i] >= 10) {
            v = val[i]
            d = dep[i]
            for (j = n; j > i; j--) {
                val[j + 1] = val[j]
                dep[j + 1] = dep[j]
            }
            n++
            val[i] = int(v / 2)
            dep[i] = d + 1
            val[i + 1] = int((v + 1) / 2)
            dep[i + 1] = d + 1
            return 1
        }
    }
    return 0
}

function reduce() {
    while (1) {
        if (explode()) continue
        if (split_val()) continue
        break
    }
}

function get_mag(i, j, mn, max_d, mv, md) {
    for (i = 1; i <= n; i++) {
        mv[i] = val[i]
        md[i] = dep[i]
    }
    mn = n
    while (mn > 1) {
        max_d = 0
        for (i = 1; i <= mn; i++) if (md[i] > max_d) max_d = md[i]
        for (i = 1; i < mn; i++) {
            if (md[i] == max_d && md[i] == md[i + 1]) {
                mv[i] = 3 * mv[i] + 2 * mv[i + 1]
                md[i]--
                for (j = i + 1; j < mn; j++) {
                    mv[j] = mv[j + 1]
                    md[j] = md[j + 1]
                }
                mn--
                break
            }
        }
    }
    return mv[1]
}

NR == 1 {
    d = 0
    s = $0
    for (i = 1; i <= length(s); i++) {
        c = substr(s, i, 1)
        if (c == "[") d++
        else if (c == "]") d--
        else if (c ~ /[0-9]/) {
            v = 0
            while (substr(s, i, 1) ~ /[0-9]/) {
                v = v * 10 + substr(s, i, 1)
                i++
            }
            i--
            n++
            val[n] = v
            dep[n] = d
        }
    }
    next
}

{
    for (i = 1; i <= n; i++) dep[i]++
    d = 0
    s = $0
    for (i = 1; i <= length(s); i++) {
        c = substr(s, i, 1)
        if (c == "[") d++
        else if (c == "]") d--
        else if (c ~ /[0-9]/) {
            v = 0
            while (substr(s, i, 1) ~ /[0-9]/) {
                v = v * 10 + substr(s, i, 1)
                i++
            }
            i--
            n++
            val[n] = v
            dep[n] = d + 1
        }
    }
    reduce()
}

END {
    print get_mag()
}