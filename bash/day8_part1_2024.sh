
#!/bin/bash
awk '
{
    w = length($0)
    for (x = 1; x <= w; x++) {
        c = substr($0, x, 1)
        if (c != ".") {
            n = ++cnt[c]
            pos_y[c, n] = NR - 1
            pos_x[c, n] = x - 1
        }
    }
    W = w
}
END {
    H = NR
    for (c in cnt) {
        n = cnt[c]
        for (i = 1; i < n; i++) {
            for (j = i + 1; j <= n; j++) {
                ay1 = 2 * pos_y[c, i] - pos_y[c, j]
                ax1 = 2 * pos_x[c, i] - pos_x[c, j]
                if (ay1 >= 0 && ay1 < H && ax1 >= 0 && ax1 < W) nodes[ay1, ax1]
                
                ay2 = 2 * pos_y[c, j] - pos_y[c, i]
                ax2 = 2 * pos_x[c, j] - pos_x[c, i]
                if (ay2 >= 0 && ay2 < H && ax2 >= 0 && ax2 < W) nodes[ay2, ax2]
            }
        }
    }
    for (node in nodes) total++
    print total + 0
}' input.txt
