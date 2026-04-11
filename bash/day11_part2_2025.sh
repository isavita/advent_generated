
#!/usr/bin/env bash
awk '
/:/{
    split($0,a,":")
    u=a[1]; gsub(/[ \t]/,"",u)
    n=split(a[2],b," ")
    for(i=1;i<=n;i++) adj[u,++deg[u]]=b[i]
}
function f(c,g,   k,i,r){
    if(c==g) return 1
    k=c SUBSEP g
    if(k in m) return m[k]
    r=0
    for(i=1;i<=deg[c];i++) r+=f(adj[c,i],g)
    return m[k]=r
}
END{
    v=f("svr","dac")*f("dac","fft")*f("fft","out")+f("svr","fft")*f("fft","dac")*f("dac","out")
    printf "%.0f\n",v
}
' input.txt
