
function d(s,i,v){
    for(i=1;i<=length(s);i++) v=v*2+substr(s,i,1)
    return v
}
function f(m,i,j,c1,c0,t,rem,b){
    for(i=1;i<=n;i++) b[i]=a[i]
    rem=n
    for(j=1;j<=L&&rem>1;j++){
        c1=0
        for(i in b) c1+=substr(b[i],j,1)
        c0=rem-c1
        t=(m=="o"?(c1>=c0?"1":"0"):(c0<=c1?"0":"1"))
        for(i in b) if(substr(b[i],j,1)!=t){delete b[i]; rem--}
    }
    for(i in b) return b[i]
}
BEGIN{
    ARGV[1]="input.txt"
    ARGC=2
}
{
    a[++n]=$0
    L=length($0)
}
END{
    print d(f("o"))*d(f("c"))
}
