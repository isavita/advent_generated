
#!/usr/bin/awk -f

function trim(s) {
    gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", s)
    return s
}

function can_make(str,    n,i,j,lp,ok) {
    n = length(str)
    delete dp
    dp[0] = 1
    for (i = 1; i <= n; i++) {
        ok = 0
        for (j = 1; j <= pat_cnt; j++) {
            lp = pat_len[j]
            if (lp == 0 && i > 0) continue
            if (i >= lp && dp[i - lp]) {
                if (substr(str, i - lp + 1, lp) == pat[j]) {
                    dp[i] = 1
                    ok = 1
                    break
                }
            }
        }
        if (!ok) dp[i] = 0
    }
    return dp[n] ? 1 : 0
}

BEGIN {
    # read whole file
    while ((getline line < "input.txt") > 0) {
        lines[++line_cnt] = line
    }
    close("input.txt")

    # first line: patterns separated by commas
    split(lines[1], tmp, ",")
    for (i = 1; i <= length(tmp); i++) {
        pat[i] = trim(tmp[i])
        pat_len[i] = length(pat[i])
    }
    pat_cnt = i - 1

    # skip second line (separator)
    # process remaining lines as designs
    count = 0
    for (i = 3; i <= line_cnt; i++) {
        design = trim(lines[i])
        if (design == "") continue
        if (can_make(design)) count++
    }

    print count
    exit
}
