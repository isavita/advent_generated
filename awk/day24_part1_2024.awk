BEGIN {
    while ((getline < "input.txt") > 0) {
        if (/:/) { sub(/:/,"",$1); w[$1]=$2 }
        else if (/->/) { n++; i1[n]=$1; o[n]=$2; i2[n]=$3; ot[n]=$5 }
    }
    do {
        f = 0
        for (i = 1; i <= n; i++) {
            if (!d[i] && i1[i] in w && i2[i] in w) {
                a=w[i1[i]]; b=w[i2[i]]; p=o[i]
                w[ot[i]] = (p=="AND" ? a&&b : (p=="OR" ? a||b : a!=b))
                d[i]=f=1
            }
        }
    } while (f)
    for (k in w) if (k ~ /^z/) r += w[k] * (2 ^ substr(k, 2))
    printf "%.0f\n", r
}