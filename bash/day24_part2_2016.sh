
#!/bin/bash
awk '
BEGIN {
    dr[1]=0; dc[1]=1; dr[2]=0; dc[2]=-1; dr[3]=1; dc[3]=0; dr[4]=-1; dc[4]=0
    pow2[0]=1; for(i=1; i<=10; i++) pow2[i]=pow2[i-1]*2
}
{
    rows++
    split($0, chars, "")
    cols=length($0)
    for(i=1; i<=cols; i++) {
        grid[rows,i]=chars[i]
        if(chars[i] ~ /[0-9]/) {
            poi_r[chars[i]]=rows; poi_c[chars[i]]=i
            if(chars[i]>max_poi) max_poi=chars[i]
        }
    }
}
function bfs(id,  qr,qc,qd,h,t,v,r,c,d,i,nr,nc) {
    h=t=1; qr[t]=poi_r[id]; qc[t]=poi_c[id]; qd[t]=0; t++
    v[poi_r[id],poi_c[id]]=1
    while(h<t) {
        r=qr[h]; c=qc[h]; d=qd[h++];
        if(grid[r,c] ~ /[0-9]/) dist[id,grid[r,c]]=d
        for(i=1; i<=4; i++) {
            nr=r+dr[i]; nc=c+dc[i]
            if(nr>=1 && nr<=rows && nc>=1 && nc<=cols && grid[nr,nc] != "#" && !v[nr,nc]) {
                v[nr,nc]=1; qr[t]=nr; qc[t]=nc; qd[t]=d+1; t++
            }
        }
    }
}
function tsp(m, cur,  i, r, v) {
    if(m == target) return dist[cur,0]
    if((m,cur) in memo) return memo[m,cur]
    r = 1e9
    for(i=0; i<n; i++) {
        if(int(m/pow2[i])%2 == 0) {
            v = dist[cur,i] + tsp(m+pow2[i], i)
            if(v < r) r = v
        }
    }
    return memo[m,cur] = r
}
END {
    n=max_poi+1; target=pow2[n]-1
    for(i=0; i<n; i++) bfs(i)
    print tsp(1, 0)
}' input.txt
