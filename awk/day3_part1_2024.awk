
BEGIN {
    RS="^$"          # slurp whole file
    getline <"input.txt"
    close("input.txt")
    while (match($0, /mul\([0-9]{1,3},[0-9]{1,3}\)/)) {
        m = substr($0, RSTART, RLENGTH)
        split(substr(m, 5, length(m)-5), a, ",")
        sum += a[1] * a[2]
        $0 = substr($0, RSTART + RLENGTH)
    }
    print sum
}
