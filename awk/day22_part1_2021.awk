
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    gsub(/[xyz]=|,|\.\./, " ")
    x1=$2; x2=$3; y1=$4; y2=$5; z1=$6; z2=$7
    if (x1 < -50 || x2 > 50 || y1 < -50 || y2 > 50 || z1 < -50 || z2 > 50) next
    for (x = x1; x <= x2; x++) {
        xo = (x + 50) * 10201
        for (y = y1; y <= y2; y++) {
            yo = xo + (y + 50) * 101
            for (z = z1; z <= z2; z++) {
                id = yo + (z + 50)
                if ($1 == "on") g[id] = 1
                else delete g[id]
            }
        }
    }
}
END {
    for (i in g) n++
    print n + 0
}
