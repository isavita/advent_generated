
function ev(s, a, i, n, r) {
    n = split(s, a)
    r = a[1]
    for (i = 2; i < n; i += 2)
        if (a[i] == "+") r += a[i+1]; else r *= a[i+1]
    return r
}
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    gsub(/\(/, " ( "); gsub(/\)/, " ) ")
    while (match($0, /\([^()]*\)/)) {
        v = ev(substr($0, RSTART + 1, RLENGTH - 2))
        $0 = substr($0, 1, RSTART - 1) " " v " " substr($0, RSTART + RLENGTH)
    }
    total += ev($0)
}
END {
    printf "%.0f\n", total
}
