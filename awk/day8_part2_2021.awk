
function s(w, i,j,n,c,t,r){
    n=split(w,c,"")
    for(i=1;i<n;i++)for(j=i+1;j<=n;j++)if(c[i]>c[j]){t=c[i];c[i]=c[j];c[j]=t}
    r=""
    for(i=1;i<=n;i++)r=r c[i]
    return r
}
function k(b,m, i){
    if(!m)return 0
    for(i=1;i<=length(m);i++)if(index(b,substr(m,i,1))==0)return 0
    return 1
}
BEGIN{ARGV[1]="input.txt";ARGC=2}
{
    delete d; delete v
    for(i=1;i<=10;i++){
        p[i]=s($i);l=length(p[i])
        if(l==2)d[1]=p[i];else if(l==3)d[7]=p[i];else if(l==4)d[4]=p[i];else if(l==7)d[8]=p[i]
    }
    for(i=1;i<=10;i++){
        l=length(p[i])
        if(l==6){if(k(p[i],d[4]))d[9]=p[i];else if(k(p[i],d[1]))d[0]=p[i];else d[6]=p[i]}
    }
    for(i=1;i<=10;i++){
        l=length(p[i])
        if(l==5){if(k(p[i],d[1]))d[3]=p[i];else if(k(d[6],p[i]))d[5]=p[i];else d[2]=p[i]}
    }
    for(i=0;i<=9;i++)v[d[i]]=i
    ans+=(v[s($12)]*1000+v[s($13)]*100+v[s($14)]*10+v[s($15)])
}
END{print ans+0}
