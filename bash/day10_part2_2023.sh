
#!/bin/bash
awk '
{
    row[H++] = $0; W = length($0)
    if (i = index($0, "S")) { sx = i - 1; sy = H - 1 }
}
END {
    if (sy > 0 && index("|7F", substr(row[sy-1], sx + 1, 1))) sp = sp "T"
    if (sy < H - 1 && index("|LJ", substr(row[sy+1], sx + 1, 1))) sp = sp "B"
    if (sx > 0 && index("-LF", substr(row[sy], sx, 1))) sp = sp "L"
    if (sx < W - 1 && index("-J7", substr(row[sy], sx + 2, 1))) sp = sp "R"
    
    split("| - J L 7 F TB LR TL TR BL BR", m)
    for (i = 1; i <= 6; i++) {
        conn[m[i]] = m[i+6]
        if (index(m[i+6], substr(sp, 1, 1)) && index(m[i+6], substr(sp, 2, 1))) sc = m[i]
    }
    
    cx = sx; cy = sy; d = substr(sp, 1, 1)
    do {
        p = (cx == sx && cy == sy) ? sc : substr(row[cy], cx + 1, 1)
        lp[cx "," cy] = p
        if (d == "T") cy--; else if (d == "B") cy++; else if (d == "L") cx--; else cx++
        p_next = (cx == sx && cy == sy) ? sc : substr(row[cy], cx + 1, 1)
        cp = conn[p_next]
        o = (d == "T" ? "B" : d == "B" ? "T" : d == "L" ? "R" : "L")
        d = (substr(cp, 1, 1) == o) ? substr(cp, 2, 1) : substr(cp, 1, 1)
    } while (cx != sx || cy != sy)

    for (j = 0; j < H; j++) {
        in_loop = 0
        for (i = 0; i < W; i++) {
            key = i "," j
            if (key in lp) {
                if (index("|LJ", lp[key])) in_loop = !in_loop
            } else if (in_loop) {
                count++
            }
        }
    }
    print count + 0
}' input.txt
