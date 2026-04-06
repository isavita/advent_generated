
awk '{for(i=1;i<=NF;i++)a[++n]=$i}
function f(c,m,i,v,h){
    c=a[p++];m=a[p++]
    for(i=1;i<=c;i++)h[i]=f()
    if(c==0)for(i=1;i<=m;i++)v+=a[p++]
    else for(i=1;i<=m;i++)v+=h[a[p++]]
    return v+0
}
END{p=1;print f()}' input.txt
