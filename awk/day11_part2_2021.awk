
function fl(r,c,  dr,dc,nr,nc,res){
    if(f[r,c])return 0
    f[r,c]=1
    res=1
    for(dr=-1;dr<=1;dr++)
        for(dc=-1;dc<=1;dc++)
            if(dr||dc){
                nr=r+dr;nc=c+dc
                if((nr,nc)in g)if(++g[nr,nc]>9)res+=fl(nr,nc)
            }
    return res
}
BEGIN{
    ARGV[1]="input.txt"
    ARGC=2
}
{
    w=split($0,a,"")
    for(i=1;i<=w;i++)g[NR,i]=a[i]
}
END{
    h=NR
    while(++s){
        delete f
        fc=0
        for(r=1;r<=h;r++)for(c=1;c<=w;c++)g[r,c]++
        for(r=1;r<=h;r++)for(c=1;c<=w;c++)if(g[r,c]>9)fc+=fl(r,c)
        for(i in f)g[i]=0
        if(fc==h*w){print s;exit}
    }
}
