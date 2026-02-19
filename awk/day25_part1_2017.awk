#!/usr/bin/awk -f
BEGIN{
    while ((getline line < "input.txt") > 0) {
        L[++N]=line
    }
    close("input.txt")
    initState=substr(L[1], length(L[1])-1, 1)
    gsub(/[^0-9]/, "", L[2])
    steps=L[2]+0
    for (i=4; i<=N; i+=10) {
        s=substr(L[i], length(L[i])-1, 1)
        v0=substr(L[i+2], length(L[i+2])-1, 1)+0
        m0=(L[i+3] ~ /right/) ? 1 : -1
        ns0=substr(L[i+4], length(L[i+4])-1, 1)
        v1=substr(L[i+6], length(L[i+6])-1, 1)+0
        m1=(L[i+7] ~ /right/) ? 1 : -1
        ns1=substr(L[i+8], length(L[i+8])-1, 1)
        S[s,0,0]=v0; S[s,0,1]=m0; S[s,0,2]=ns0
        S[s,1,0]=v1; S[s,1,1]=m1; S[s,1,2]=ns1
    }
    state=initState; cursor=0; sum=0
    for (c=0; c<steps; c++) {
        val = (cursor in T) ? T[cursor] : 0
        newVal=S[state,val,0]; move=S[state,val,1]; nxt=S[state,val,2]
        T[cursor]=newVal
        cursor+=move
        state=nxt
    }
    for (p in T) sum+=T[p]
    print sum
}