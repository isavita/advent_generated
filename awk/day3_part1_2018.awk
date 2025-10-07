
BEGIN {
    while ((getline < "input.txt") > 0) {
        split($0, a, /[^0-9]+/)
        id = a[2]; l = a[3]; t = a[4]; w = a[5]; h = a[6]
        for (i = l; i < l + w; i++)
            for (j = t; j < t + h; j++)
                cnt[i","j]++
    }
    for (k in cnt)
        if (cnt[k] > 1) ans++
    print ans
}
