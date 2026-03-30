
#!/bin/bash

# Calculate the number of valid points in the file
N=$(awk -F'[, ]+' 'NF>=3{c++} END{print c}' input.txt)

# 1. Generate all pair distances
# 2. Sort by distance and take the 1000 shortest
# 3. Perform Union-Find to group points into components
# 4. Calculate component sizes, sort them, and multiply the top three
awk -F'[, ]+' '
NF>=3 {
    x[n]=$1; y[n]=$2; z[n]=$3; n++
}
END {
    for (i=0; i<n; i++) {
        for (j=i+1; j<n; j++) {
            dx=x[i]-x[j]; dy=y[i]-y[j]; dz=z[i]-z[j]
            print dx*dx + dy*dy + dz*dz, i, j
        }
    }
}' input.txt | sort -n | head -n 1000 | awk -v n="$N" '
function find(i, r, t) {
    r = i
    while (p[r] != r) r = p[r]
    while (p[i] != r) { t = p[i]; p[i] = r; i = t }
    return r
}
BEGIN {
    for (i=0; i<n; i++) { p[i]=i; s[i]=1 }
}
{
    r1 = find($2); r2 = find($3)
    if (r1 != r2) {
        if (s[r1] < s[r2]) { t=r1; r1=r2; r2=t }
        p[r2] = r1; s[r1] += s[r2]
    }
}
END {
    for (i=0; i<n; i++) if (p[i] == i) print s[i]
}' | sort -rn | head -n 3 | awk '
BEGIN { res=1 }
{ res *= $1 }
END { printf "Product of three largest circuit sizes: %.0f\n", res }'
