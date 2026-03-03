BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    h++
    w = length($0)
    for (i = 1; i <= w; i++) {
        g[h, i] = substr($0, i, 1)
        if (g[h, i] == "S") {
            sx = i
            sy = h
        }
    }
}
END {
    if (sy > 1 && g[sy - 1, sx] ~ /[|7F]/) { dx = 0; dy = -1 }
    else if (sx < w && g[sy, sx + 1] ~ /[-J7]/) { dx = 1; dy = 0 }
    else if (sy < h && g[sy + 1, sx] ~ /[|LJ]/) { dx = 0; dy = 1 }
    else { dx = -1; dy = 0 }
    cx = sx + dx; cy = sy + dy; px = sx; py = sy; s = 1
    while (cx != sx || cy != sy) {
        s++
        v = g[cy, cx]
        if (v ~ /[|LJ]/ && (cy - 1 != py || cx != px)) { nx = cx; ny = cy - 1 }
        else if (v ~ /[LF-]/ && (cy != py || cx + 1 != px)) { nx = cx + 1; ny = cy }
        else if (v ~ /[|7F]/ && (cy + 1 != py || cx != px)) { nx = cx; ny = cy + 1 }
        else { nx = cx - 1; ny = cy }
        px = cx; py = cy; cx = nx; cy = ny
    }
    print s / 2
}