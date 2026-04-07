
#!/bin/bash
awk -F, '{
    for(i=1;i<=NF;i++)p[i-1]=$i
    d[1,0]=0;d[1,1]=-1;d[2,0]=0;d[2,1]=1;d[3,0]=-1;d[3,1]=0;d[4,0]=1;d[4,1]=0
    o[1]=2;o[2]=1;o[3]=4;o[4]=3;g[0,0]=1;pc=rl=0;f(0,0);print b(0,0,tx,ty)
}
function v(k){return p[k]+0}
function s(inp,ins,op,m1,m2,m3,q1,q2,q3,z){
    while(1){
        ins=p[pc];op=ins%100;m1=int(ins/100)%10;m2=int(ins/1000)%10;m3=int(ins/10000)%10
        q1=(m1==1?pc+1:(m1==2?rl+p[pc+1]:p[pc+1]));q2=(m2==1?pc+2:(m2==2?rl+p[pc+2]:p[pc+2]));q3=(m3==1?pc+3:(m3==2?rl+p[pc+3]:p[pc+3]))
        if(op==1){p[q3]=v(q1)+v(q2);pc+=4}
        else if(op==2){p[q3]=v(q1)*v(q2);pc+=4}
        else if(op==3){p[q1]=inp;pc+=2}
        else if(op==4){z=v(q1);pc+=2;return z}
        else if(op==5){pc=(v(q1)!=0?v(q2):pc+3)}
        else if(op==6){pc=(v(q1)==0?v(q2):pc+3)}
        else if(op==7){p[q3]=(v(q1)<v(q2)?1:0);pc+=4}
        else if(op==8){p[q3]=(v(q1)==v(q2)?1:0);pc+=4}
        else if(op==9){rl+=v(q1);pc+=2}
        else if(op==99)return -1
    }
}
function f(x,y,i,nx,ny,r){
    vst[x,y]=1
    for(i=1;i<=4;i++){
        nx=x+d[i,0];ny=y+d[i,1]
        if(!((nx,ny) in vst)){
            r=s(i);vst[nx,ny]=1;g[nx,ny]=r
            if(r>0){if(r==2){tx=nx;ty=ny};f(nx,ny);s(o[i])}
        }
    }
}
function b(sx,sy,tx,ty,h,t,qx,qy,qd,cx,cy,cd,i,nx,ny,v_b){
    h=t=1;qx[1]=sx;qy[1]=sy;qd[1]=0;v_b[sx,sy]=1
    while(h<=t){
        cx=qx[h];cy=qy[h];cd=qd[h++];if(cx==tx&&cy==ty)return cd
        for(i=1;i<=4;i++){
            nx=cx+d[i,0];ny=cy+d[i,1]
            if(g[nx,ny]>0&&!((nx,ny) in v_b)){v_b[nx,ny]=1;t++;qx[t]=nx;qy[t]=ny;qd[t]=cd+1}
        }
    }
}' input.txt
