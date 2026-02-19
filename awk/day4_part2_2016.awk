
#!/usr/bin/awk -f
BEGIN {
    while ((getline line < "input.txt") > 0) {
        sub(/\r?\n$/, "", line)
        split(line, left, "[")
        encrypted = left[1]
        checksum = substr(left[2], 1, length(left[2]) - 1)
        n = split(encrypted, parts, "-")
        sector = parts[n]
        delete parts[n]
        delete cnt
        for (i = 1; i <= n - 1; i++) {
            name = parts[i]
            for (j = 1; j <= length(name); j++) {
                ch = substr(name, j, 1)
                cnt[ch]++
            }
        }
        calc = ""
        delete used
        for (k = 1; k <= 5; k++) {
            max = -1; sel = ""
            for (c = 97; c <= 122; c++) {
                ch = sprintf("%c", c)
                if (used[ch]) continue
                cur = cnt[ch] + 0
                if (cur > max || (cur == max && ch < sel)) {
                    max = cur; sel = ch
                }
            }
            calc = calc sel
            used[sel] = 1
        }
        if (calc != checksum) continue
        shift = sector % 26
        dec = ""
        for (i = 1; i <= n - 1; i++) {
            name = parts[i]
            for (j = 1; j <= length(name); j++) {
                ch = substr(name, j, 1)
                dec = dec sprintf("%c", (ord(ch) - 97 + shift) % 26 + 97)
            }
            dec = dec " "
        }
        sub(/[ ]+$/, "", dec)
        if (index(dec, "northpole object") > 0) {
            print sector
            exit
        }
    }
    exit
}
function ord(c) { return sprintf("%d", index("abcdefghijklmnopqrstuvwxyz", c) + 96) }
