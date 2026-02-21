
#!/usr/bin/awk -f

# ------------------------------------------------------------
# One‑Time Pad (Advent of Code 2016 – Day 14) in pure awk
# Reads the salt from "input.txt" and prints the index of the
# 64th valid key.
# ------------------------------------------------------------

BEGIN {
    # read the salt
    getline salt < "input.txt"
    gsub(/[ \r\n]+/, "", salt)

    keys = 0
    idx  = 0

    while (keys < 64) {
        if (!(idx in hcache))
            hcache[idx] = md5(salt idx)

        h = hcache[idx]
        t = triplet(h)

        if (t != "") {
            quint = t t t t t
            for (i = 1; i <= 1000; i++) {
                j = idx + i
                if (!(j in hcache))
                    hcache[j] = md5(salt j)

                if (index(hcache[j], quint) > 0) {
                    keys++
                    if (keys == 64) {
                        print idx
                        exit
                    }
                    break
                }
            }
        }
        idx++
    }
}

# ------------------------------------------------------------
# Return the MD5 hash (hex) of the given string.
# Uses the external `md5sum` command; works with GNU coreutils.
# ------------------------------------------------------------
function md5(str,   cmd, out, a) {
    # printf %s avoids adding a newline
    cmd = "printf \"%s\" \"" str "\" | md5sum"
    cmd | getline out
    close(cmd)
    split(out, a, " ")
    return a[1]
}

# ------------------------------------------------------------
# Return the first character that appears three times consecutively.
# Empty string if none.
# ------------------------------------------------------------
function triplet(h,   i, c) {
    for (i = 1; i <= length(h) - 2; i++) {
        c = substr(h, i, 1)
        if (c == substr(h, i+1, 1) && c == substr(h, i+2, 1))
            return c
    }
    return ""
}
