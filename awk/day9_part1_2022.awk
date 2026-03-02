
function abs(x) { return x < 0 ? -x : x }
BEGIN {
    hx = hy = tx = ty = 0
    v[0,0] = 1
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    for (i = 0; i < $2; i++) {
        if ($1 == "R") hx++
        else if ($1 == "L") hx--
        else if ($1 == "U") hy++
        else if ($1 == "D") hy--
        dx = hx - tx
        dy = hy - ty
        if (abs(dx) > 1 || abs(dy) > 1) {
            tx += (dx > 0 ? 1 : (dx < 0 ? -1 : 0))
            ty += (dy > 0 ? 1 : (dy < 0 ? -1 : 0))
        }
        v[tx,ty] = 1
    }
}
END {
    for (i in v) n++
    print n
}
