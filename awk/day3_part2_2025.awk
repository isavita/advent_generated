
#!/usr/bin/awk -f
# k‑length maximum subsequence sum (big‑int) for each line of input.txt

function maxSubseq(s, k,   n, toRemove, stack, top, i, d, result) {
    n = length(s)
    toRemove = n - k
    delete stack
    top = 0
    for (i = 1; i <= n; i++) {
        d = substr(s, i, 1)
        while (toRemove > 0 && top > 0 && stack[top] < d) {
            delete stack[top]
            top--
            toRemove--
        }
        top++
        stack[top] = d
    }
    result = ""
    for (i = 1; i <= k; i++) result = result stack[i]
    return result
}

function addBig(a, b,   i, carry, da, db, sum, res) {
    i = 0; carry = 0; res = ""
    while (i < length(a) || i < length(b) || carry) {
        i++
        da = (i <= length(a)) ? substr(a, length(a)-i+1, 1)+0 : 0
        db = (i <= length(b)) ? substr(b, length(b)-i+1, 1)+0 : 0
        sum = da + db + carry
        res = (sum % 10) res
        carry = int(sum / 10)
    }
    sub(/^0+/, "", res)
    if (res == "") res = "0"
    return res
}

BEGIN {
    k = 12
    total = "0"
    while ((getline line < "input.txt") > 0) {
        gsub(/^[ \t\r\n]+|[ \t\r\n]+$/, "", line)
        if (length(line) >= k) {
            best = maxSubseq(line, k)
            total = addBig(total, best)
        }
    }
    close("input.txt")
    print "Total output joltage: " total
}
