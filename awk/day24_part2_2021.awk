
BEGIN {
    while ((getline < "input.txt") > 0) {
        if ($1=="div" && $2=="z") L[int(N/18)] = $3
        if ($1=="add" && $2=="x") K[int(N/18)] = $3
        if ($1=="add" && $2=="y") M[int(N/18)] = $3
        N++
    }
    close("input.txt")
    for (i=0; i<14; i++) {
        if (L[i]==1) { S[top++]=i }
        else {
            p=S[--top]
            C[p,0]=i
            C[p,1]=M[p]+K[i]
        }
    }
    for (i=0; i<14; i++) {
        if (C[i,0]) {
            v=1
            while (v+C[i,1]<1) v++
            min[i]=v
            min[C[i,0]]=v+C[i,1]
        }
    }
    for (i=0; i<14; i++) n=n*10+min[i]
    print n
}
