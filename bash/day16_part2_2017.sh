
#!/bin/bash

awk -F, '
function g(r,k){r="";for(k=0;k<16;k++)r=r p[(k+o)%16];return r}
function d(m,c,i,a,i1,i2,t,j){
    for(i=1;i<=n;i++){
        m=v[i];c=substr(m,1,1);split(substr(m,2),a,"/")
        if(c=="s")o=(o+16-a[1]%16)%16
        else{
            if(c=="x"){i1=(a[1]+o)%16;i2=(a[2]+o)%16}
            else for(j=0;j<16;j++){if(p[j]==a[1])i1=j;if(p[j]==a[2])i2=j}
            t=p[i1];p[i1]=p[i2];p[i2]=t
        }
    }
}
BEGIN{for(i=0;i<16;i++)p[i]=sprintf("%c",97+i)}
{
    n=NF;for(i=1;i<=n;i++)v[i]=$i
    d();r1=g();print "Part 1: " r1
    m[r1]=1;h[1]=r1
    for(it=2;it<=1000000000;it++){
        d();r=g()
        if(r in m){
            s=m[r];l=it-s
            print "Part 2: " h[s+(1000000000-s)%l];exit
        }
        m[r]=it;h[it]=r
    }
}' input.txt
