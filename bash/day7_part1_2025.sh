
#!/usr/bin/env bash

awk '
BEGIN { ARGV[1] = "input.txt"; ARGC = 2 }
NF {
    r[++h] = $0
    if (i = index($0, "S")) { sx = i; sy = h }
}
END {
    if (!h) { print "Empty grid"; exit }

    if (sy) {
        act[sx] = 1
        for (y = sy; y <= h; y++) {
            delete nxt; has = 0
            for (x in act) {
                has = 1
                c = substr(r[y], x, 1)
                if (c == "^") {
                    s++
                    if (x > 1) nxt[x-1] = 1
                    if (x < length(r[y])) nxt[x+1] = 1
                } else {
                    nxt[x] = 1
                }
            }
            if (!has) break
            delete act
            for (x in nxt) act[x] = 1
        }
    }

    print "Total times the beam is split:", s + 0
}
' input.txt
