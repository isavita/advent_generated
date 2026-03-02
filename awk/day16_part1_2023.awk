
BEGIN {
    dx["R"]=1; dx["L"]=-1; dy["U"]=-1; dy["D"]=1
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    W = length($0)
    H = NR
    for (i = 1; i <= W; i++) g[NR-1, i-1] = substr($0, i, 1)
}
function s(nx, ny, nd) {
    if (nx >= 0 && nx < W && ny >= 0 && ny < H) q[++t] = nx "," ny "," nd
}
END {
    s(0, 0, "R")
    while (t > 0) {
        c_s = q[t--]
        if (v[c_s]++) continue
        split(c_s, a, ",")
        x = a[1]; y = a[2]; d = a[3]; e[x, y] = 1; c = g[y, x]
        if (c == ".") s(x + dx[d], y + dy[d], d)
        else if (c == "/") {
            n = (d == "R" ? "U" : d == "L" ? "D" : d == "U" ? "R" : "L")
            s(x + dx[n], y + dy[n], n)
        } else if (c == "\\") {
            n = (d == "R" ? "D" : d == "L" ? "U" : d == "U" ? "L" : "R")
            s(x + dx[n], y + dy[n], n)
        } else if (c == "|") {
            if (d == "R" || d == "L") { s(x, y - 1, "U"); s(x, y + 1, "D") }
            else s(x + dx[d], y + dy[d], d)
        } else if (c == "-") {
            if (d == "U" || d == "D") { s(x - 1, y, "L"); s(x + 1, y, "R") }
            else s(x + dx[d], y + dy[d], d)
        }
    }
    for (i in e) r++
    print r + 0
}

