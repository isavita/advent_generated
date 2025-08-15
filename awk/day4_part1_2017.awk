
#!/usr/bin/awk -f

BEGIN {
    valid = 0
    FS = "[ \t\r\n]+"
    while ((getline line < "input.txt") > 0) {
        delete seen
        dup = 0
        n = split(line, words, FS)
        for (i = 1; i <= n; i++) {
            if (words[i] in seen) { dup = 1; break }
            seen[words[i]]
        }
        if (!dup) ++valid
    }
    close("input.txt")
    print valid
    exit
}
