
#!/bin/bash
awk -F, '{
    for(i=0; i<NF; i++) m[i] = $(i+1)
    ip=rb=x=y=a=0
    while(m[ip] != 99) {
        p=m[ip]; o=p%100; m1=int(p/100)%10; m2=int(p/1000)%10; m3=int(p/10000)%10
        v1=(m1==0?m[m[ip+1]+0]:m1==1?m[ip+1]:m[rb+m[ip+1]])+0
        v2=(m2==0?m[m[ip+2]+0]:m2==1?m[ip+2]:m[rb+m[ip+2]])+0
        t1=(m1==2?rb+m[ip+1]:m[ip+1])+0; t3=(m3==2?rb+m[ip+3]:m[ip+3])+0
        if(o==1){m[t3]=v1+v2;ip+=4}
        else if(o==2){m[t3]=v1*v2;ip+=4}
        else if(o==3){m[t1]=0;ip+=2}
        else if(o==4){if(v1==10){y++;x=0}else{if(v1!=46)s[x,y]=1;x++};ip+=2}
        else if(o==5){ip=(v1!=0?v2:ip+3)}
        else if(o==6){ip=(v1==0?v2:ip+3)}
        else if(o==7){m[t3]=(v1<v2?1:0);ip+=4}
        else if(o==8){m[t3]=(v1==v2?1:0);ip+=4}
        else if(o==9){rb+=v1;ip+=2}
        else break
    }
    for(k in s){
        split(k,c,SUBSEP);cx=c[1];cy=c[2]
        if((cx+1,cy)in s && (cx-1,cy)in s && (cx,cy+1)in s && (cx,cy-1)in s) a+=cx*cy
    }
    print a
}' input.txt
