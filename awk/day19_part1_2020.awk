BEGIN {
    FS = ": "
    ARGV[1] = "input.txt"
    ARGC = 2
}

!msg && /:/ {
    rules[$1] = $2
    next
}

$0 == "" && !msg {
    msg = 1
    r0 = "^" expand("0") "$"
    next
}

msg && NF {
    if ($0 ~ r0) count++
}

END {
    print count + 0
}

function expand(id, def, alts, n, i, res, subp, m, j) {
    if (id in memo) return memo[id]
    def = rules[id]
    if (def ~ /"/) {
        gsub(/[ "]/, "", def)
        return memo[id] = def
    }
    n = split(def, alts, /\|/)
    res = "("
    for (i = 1; i <= n; i++) {
        res = res (i > 1 ? "|" : "")
        m = split(alts[i], subp, " ")
        for (j = 1; j <= m; j++)
            res = res expand(subp[j])
    }
    return memo[id] = res ")"
}