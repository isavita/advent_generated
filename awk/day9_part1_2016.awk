
#!/usr/bin/awk -f
BEGIN {
    # Read entire file into variable s
    s = ""
    while ((getline line < "input.txt") > 0) s = s line
    close("input.txt")

    len = 0
    i = 1
    n = length(s)

    while (i <= n) {
        c = substr(s, i, 1)
        if (c == "(") {
            j = i + 1
            while (substr(s, j, 1) ~ /[0-9]/) j++
            numChars = substr(s, i + 1, j - i - 1)
            j++  # skip 'x'
            k = j
            while (substr(s, k, 1) ~ /[0-9]/) k++
            repeat = substr(s, j, k - j)
            i = k + 1  # skip ')'
            len += numChars * repeat
            i += numChars
        } else if (c !~ /[[:space:]]/) {
            len++
            i++
        } else {
            i++
        }
    }

    print len
}
