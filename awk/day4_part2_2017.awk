#!/usr/bin/awk -f
function sortword(s,   i, j, n, a, t) {
    n = length(s)
    for (i = 1; i <= n; i++) a[i] = substr(s, i, 1)
    for (i = 1; i <= n-1; i++) {
        for (j = i+1; j <= n; j++) {
            if (a[i] > a[j]) { t = a[i]; a[i] = a[j]; a[j] = t }
        }
    }
    sw = ""
    for (i = 1; i <= n; i++) sw = sw a[i]
    for (i in a) delete a[i]
    return sw
}
BEGIN {
    file = "input.txt"
    validCount = 0
    while ((getline line < file) > 0) {
        n = split(line, words, /[[:space:]]+/)
        valid = 1
        delete seen
        for (i = 1; i <= n; i++) {
            w = words[i]
            if (w == "") continue
            sw = sortword(w)
            if (sw in seen) {
                valid = 0
                break
            }
            seen[sw] = 1
        }
        if (valid) validCount++
    }
    close(file)
    print validCount
    exit
}