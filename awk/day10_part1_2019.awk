
BEGIN {
    while ((getline < "input.txt") > 0) {
        ++H
        W = length($0)
        for (x = 1; x <= W; ++x)
            A[H,x] = substr($0, x, 1) == "#"
    }
    close("input.txt")

    for (y = 1; y <= H; ++y) {
        for (x = 1; x <= W; ++x) {
            if (!A[y,x]) continue
            delete seen
            cnt = 0
            for (oy = 1; oy <= H; ++oy) {
                for (ox = 1; ox <= W; ++ox) {
                    if (!A[oy,ox] || (ox == x && oy == y)) continue
                    dx = ox - x
                    dy = oy - y
                    g = gcd(dx, dy)
                    sx = dx / g
                    sy = dy / g
                    key = sx "," sy
                    if (!seen[key]) {
                        seen[key] = 1
                        ++cnt
                    }
                }
            }
            if (cnt > best) best = cnt
        }
    }
    print best
}
function gcd(a, b) {
    a = a < 0 ? -a : a
    b = b < 0 ? -b : b
    while (b) {
        t = b
        b = a % b
        a = t
    }
    return a
}
