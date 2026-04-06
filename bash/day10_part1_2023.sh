
#!/bin/bash
awk '
{
    g[NR] = $0
    if (i = index($0, "S")) { sx = i; sy = NR }
}
END {
    w = length(g[1]); h = NR
    if (sy > 1 && substr(g[sy-1], sx, 1) ~ /[|7F]/) { dx = 0; dy = -1 }
    else if (sx < w && substr(g[sy], sx+1, 1) ~ /[-J7]/) { dx = 1; dy = 0 }
    else if (sy < h && substr(g[sy+1], sx, 1) ~ /[|LJ]/) { dx = 0; dy = 1 }
    else { dx = -1; dy = 0 }
    
    cx = sx + dx; cy = sy + dy; px = sx; py = sy; s = 1
    while (cx != sx || cy != sy) {
        s++
        v = substr(g[cy], cx, 1)
        if (v ~ /[|LJ]/ && cy - 1 != py) { nx = cx; ny = cy - 1 }
        else if (v ~ /[LF-]/ && cx + 1 != px) { nx = cx + 1; ny = cy }
        else if (v ~ /[|7F]/ && cy + 1 != py) { nx = cx; ny = cy + 1 }
        else { nx = cx - 1; ny = cy }
        px = cx; py = cy; cx = nx; cy = ny
    }
    print s / 2
}' input.txt
