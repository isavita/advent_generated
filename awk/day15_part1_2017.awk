
BEGIN {
    while ((getline < "input.txt") > 0) v[++n] = $NF
    a = v[1]
    b = v[2]
    m = 2147483647
    for (i = 0; i < 40000000; i++) {
        a = (a * 16807) % m
        b = (b * 48271) % m
        if (a % 65536 == b % 65536) c++
    }
    print c + 0
}
