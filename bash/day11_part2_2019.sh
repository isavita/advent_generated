
#!/bin/bash
awk -F, '{for(i=0;i<NF;i++)a[i]=$ (i+1)} END {
    for(s=0;s<=1;s++) {
        split("",m); for(k in a)m[k]=a[k]
        split("",c); split("",p); x=y=d=i=r=q=0; c[0,0]=s
        while(1){
            o=m[i]%100; m1=int(m[i]/100)%10; m2=int(m[i]/1000)%10; m3=int(m[i]/10000)%10
            if(o==99) break
            v1=(m1==1?m[i+1]:(m1==2?m[r+m[i+1]]:m[m[i+1]]))+0
            v2=(m2==1?m[i+2]:(m2==2?m[r+m[i+2]]:m[m[i+2]]))+0
            a1=(m1==2?r+m[i+1]:m[i+1]); a3=(m3==2?r+m[i+3]:m[i+3])
            if(o==1){m[a3]=v1+v2;i+=4}
            else if(o==2){m[a3]=v1*v2;i+=4}
            else if(o==3){m[a1]=c[x,y];i+=2}
            else if(o==4){
                if(q==0){c[x,y]=v1;p[x,y]=1;q=1}
                else{d=(v1==0?(d+3)%4:(d+1)%4);if(d==0)y--;else if(d==1)x++;else if(d==2)y++;else x--;q=0}
                i+=2
            }
            else if(o==5)i=(v1!=0?v2:i+3)
            else if(o==6)i=(v1==0?v2:i+3)
            else if(o==7){m[a3]=(v1<v2?1:0);i+=4}
            else if(o==8){m[a3]=(v1==v2?1:0);i+=4}
            else if(o==9){r+=v1;i+=2}
        }
        if(s==0){for(k in p)t++; print "Part One: " t}
        else {
            print "Part Two:"; x0=x1=y0=y1=0
            for(k in c){split(k,g,SUBSEP);if(g[1]<x0)x0=g[1];if(g[1]>x1)x1=g[1];if(g[2]<y0)y0=g[2];if(g[2]>y1)y1=g[2]}
            for(j=y0;j<=y1;j++){l="";for(k=x0;k<=x1;k++)l=l (c[k,j]?"#":" ");print l}
        }
    }
}' input.txt
