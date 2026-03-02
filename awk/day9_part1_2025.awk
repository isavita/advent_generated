
BEGIN {
    FS = ","
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    x[++n] = $1
    y[n] = $2
}
END {
    for (i = 1; i <= n; i++) {
        xi = x[i]; yi = y[i]
        for (j = i; j <= n; j++) {
            w = (xi > x[j] ? xi - x[j] : x[j] - xi) + 1
            h = (yi > y[j] ? yi - y[j] : y[j] - yi) + 1
            if (w * h > max) max = w * h
        }
    }
    print max + 0
}
