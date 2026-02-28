
BEGIN{
    while ((getline line < "input.txt") > 0) {
        lines[NR]=line; NR++
    }
    R=NR
    if (R==0) { print 0; exit }
    C=length(lines[0])
    sr=-1; sc=-1
    for (r=0; r<R && sr==-1; r++) {
        pos=index(lines[r],"S")
        if (pos) { sr=r; sc=pos-1 }
    }
    if (sr==-1) { print 0; exit }
    dp[sr,sc]=1
    total=0
    for (r=0; r<R; r++) {
        for (c=0; c<C; c++) {
            v=dp[r,c]+0
            if (v==0) continue
            nr=r+1
            if (nr==R) { total+=v; continue }
            ch=substr(lines[nr],c+1,1)
            if (ch=="^") {
                if (c-1<0) total+=v; else dp[nr,c-1]+=v
                if (c+1>=C) total+=v; else dp[nr,c+1]+=v
            } else {
                dp[nr,c]+=v
            }
        }
    }
    print total
}
