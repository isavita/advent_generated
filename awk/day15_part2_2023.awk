
BEGIN {
    RS = ","
    for (i = 0; i < 256; i++) ord[sprintf("%c", i)] = i
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    gsub(/[ \n\r]/, "", $0)
    if ($0 ~ /-/) {
        l = substr($0, 1, length($0) - 1)
        b = h(l)
        for (i = 1; i <= c[b]; i++) {
            if (bl[b, i] == l) {
                for (j = i; j < c[b]; j++) bl[b, j] = bl[b, j+1]
                delete bl[b, c[b]--]
                break
            }
        }
    } else if ($0 ~ /=/) {
        split($0, a, "=")
        l = a[1]; v = a[2]; b = h(l); f = 0
        for (i = 1; i <= c[b]; i++) {
            if (bl[b, i] == l) {
                fv[l] = v; f = 1; break
            }
        }
        if (!f) {
            bl[b, ++c[b]] = l
            fv[l] = v
        }
    }
}
function h(s, _h, _k) {
    _h = 0
    for (_k = 1; _k <= length(s); _k++) _h = ((_h + ord[substr(s, _k, 1)]) * 17) % 256
    return _h
}
END {
    for (b = 0; b < 256; b++)
        for (i = 1; i <= c[b]; i++)
            tot += (b + 1) * i * fv[bl[b, i]]
    print tot
}
