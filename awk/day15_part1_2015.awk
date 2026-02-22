#!/usr/bin/awk -f
BEGIN{
    while ((getline line < "input.txt") > 0) {
        split(line, a, /[ ,:]+/)
        cap[++cnt] = a[3]+0
        dur[cnt] = a[5]+0
        flav[cnt] = a[7]+0
        tex[cnt] = a[9]+0
    }
    n = cnt
    print dfs(1,100,0,0,0,0)
}
function dfs(i,rem,c,d,f,t,   max,val,k){
    if(i==n){
        c+=cap[i]*rem
        d+=dur[i]*rem
        f+=flav[i]*rem
        t+=tex[i]*rem
        if(c<0)c=0
        if(d<0)d=0
        if(f<0)f=0
        if(t<0)t=0
        return c*d*f*t
    }
    max=0
    for(k=0;k<=rem;k++){
        val=dfs(i+1,rem-k,c+cap[i]*k,d+dur[i]*k,f+flav[i]*k,t+tex[i]*k)
        if(val>max)max=val
    }
    return max
}