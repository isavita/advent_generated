
BEGIN { ARGV[1]="input.txt"; ARGC=2 }
NR==1 { split($0,a,""); for(i=0;i<512;i++) alg[i]=(a[i+1]=="#") }
NR>2 && NF { h=NR-2; w=length($0); split($0,r,""); for(x=1;x<=w;x++) if(r[x]=="#") g[h,x]=1 }
END {
    y1=1; y2=h; x1=1; x2=w; inf=0
    for(s=1; s<=50; s++) {
        ny1=y1-1; ny2=y2+1; nx1=x1-1; nx2=x2+1
        for(y=ny1; y<=ny2; y++) {
            for(x=nx1; x<=nx2; x++) {
                v=0; for(i=-1; i<=1; i++) for(j=-1; j<=1; j++) {
                    py=y+i; px=x+j; v=v*2+((py<y1||py>y2||px<x1||px>x2)?inf:((py,px) in g))
                }
                if(alg[v]) t[y,x]=1
            }
        }
        delete g; for(k in t) g[k]=1; delete t
        inf=(inf?alg[511]:alg[0]); y1=ny1; y2=ny2; x1=nx1; x2=nx2
    }
    for(k in g) c++; print c
}
