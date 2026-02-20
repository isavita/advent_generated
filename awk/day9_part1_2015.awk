BEGIN {
    while (getline < "input.txt" > 0) {
        a=$1; b=$3; d=$5
        dist[a SUBSEP b] = d
        dist[b SUBSEP a] = d
        locs[a]=locs[b]=1
    }
    n=0
    for (l in locs) names[n++]=l
    best=1e9
    perm(0)
    print best
}
function perm(k,    i,t,sum) {
    if (k==n) {
        sum=0
        for(i=1;i<n;i++) sum+=dist[names[i-1] SUBSEP names[i]]
        if (sum<best) best=sum
        return
    }
    for (i=k;i<n;i++) {
        t=names[k]; names[k]=names[i]; names[i]=t
        perm(k+1)
        t=names[k]; names[k]=names[i]; names[i]=t
    }
}