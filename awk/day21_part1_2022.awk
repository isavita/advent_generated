
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    sub(":", "", $1)
    j1[$1] = $2
    j2[$1] = $3
    j3[$1] = $4
}
function c(n,    x, y) {
    if (n in m) return m[n]
    if (j2[n] == "") return m[n] = j1[n]
    x = c(j1[n])
    y = c(j3[n])
    if (j2[n] == "+") return m[n] = x + y
    if (j2[n] == "-") return m[n] = x - y
    if (j2[n] == "*") return m[n] = x * y
    if (j2[n] == "/") return m[n] = int(x / y)
}
END {
    printf "%.0f\n", c("root")
}
