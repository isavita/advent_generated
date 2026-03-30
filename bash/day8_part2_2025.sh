
#!/bin/bash

# Extract total count of points
N=$(awk -F',' 'NF>=3{c++}END{print c}' input.txt)

# Calculate pairwise distances, sort, and apply Kruskal's algorithm using Union-Find in AWK
awk -F',' 'NF>=3{x[n]=$1;y[n]=$2;z[n]=$3;n++}
END{for(i=0;i<n;i++)for(j=i+1;j<n;j++)print (x[i]-x[j])^2+(y[i]-y[j])^2+(z[i]-z[j])^2,i,j,x[i],y[i],z[i],x[j],y[j],z[j]}' input.txt | \
sort -n | \
awk -v n="$N" '
function find(i) {
    if (p[i] == i) return i;
    return p[i] = find(p[i]);
}
BEGIN {
    for(i=0; i<n; i++) p[i]=i;
    comps=n;
}
{
    u=find($2); v=find($3);
    if (u != v) {
        p[u] = v;
        if (--comps == 1) {
            printf "Connected %d,%d,%d and %d,%d,%d\n", $4, $5, $6, $7, $8, $9;
            printf "Product of X coordinates: %d\n", $4 * $7;
            exit;
        }
    }
}'
