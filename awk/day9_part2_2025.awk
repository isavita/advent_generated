
#!/usr/bin/awk -f
function abs(x){return x<0?-x:x}
function getSum(x1,y1,x2,y2, lx,rx,ly,ry,res){
    lx=(x1<x2?x1:x2);rx=(x1>x2?x1:x2)
    ly=(y1<y2?y1:y2);ry=(y1>y2?y1:y2)
    res=S[ry,rx]
    if(lx>0)res-=S[ry,lx-1]
    if(ly>0)res-=S[ly-1,rx]
    if(lx>0&&ly>0)res+=S[ly-1,lx-1]
    return res
}
BEGIN{
    ARGC=2;ARGV[1]="input.txt";FS=","
}
{
    if(NF<2)next
    nP++;PX[nP]=$1+0;PY[nP]=$2+0
    if(!($1+0 in hX)){hX[$1+0];UX[++nx]=$1+0}
    if(!($2+0 in hY)){hY[$2+0];UY[++ny]=$2+0}
}
END{
    if(nP==0){print "No points found.";exit}
    for(i=1;i<=nx;i++)for(j=i+1;j<=nx;j++)if(UX[i]>UX[j]){t=UX[i];UX[i]=UX[j];UX[j]=t}
    for(i=1;i<=ny;i++)for(j=i+1;j<=ny;j++)if(UY[i]>UY[j]){t=UY[i];UY[i]=UY[j];UY[j]=t}
    for(i=1;i<=nx;i++)mX[UX[i]]=2*i-1
    for(i=1;i<=ny;i++)mY[UY[i]]=2*i-1
    for(i=1;i<=nx;i++){CW[2*i-1]=1;if(i<nx)CW[2*i]=UX[i+1]-UX[i]-1}
    CW[0]=CW[2*nx]=1
    for(i=1;i<=ny;i++){RH[2*i-1]=1;if(i<ny)RH[2*i]=UY[i+1]-UY[i]-1}
    RH[0]=RH[2*ny]=1
    for(i=1;i<=nP;i++){
        p1=i;p2=(i%nP)+1
        gx1=mX[PX[p1]];gy1=mY[PY[p1]];gx2=mX[PX[p2]];gy2=mY[PY[p2]]
        if(gx1==gx2){
            s=(gy1<gy2?gy1:gy2);e=(gy1>gy2?gy1:gy2)
            for(y=s;y<=e;y++)if(RH[y]>0)G[y,gx1]=1
        }else{
            s=(gx1<gx2?gx1:gx2);e=(gx1>gx2?gx1:gx2)
            for(x=s;x<=e;x++)if(CW[x]>0)G[gy1,x]=1
        }
    }
    mW=2*nx;mH=2*ny;qx[1]=qy[1]=0;h=1;t=2;G[0,0]=2
    while(h<t){
        cx=qx[h];cy=qy[h++]
        if(cx+1<=mW&&G[cy,cx+1]==0){G[cy,cx+1]=2;qx[t]=cx+1;qy[t++]=cy}
        if(cx-1>=0&&G[cy,cx-1]==0){G[cy,cx-1]=2;qx[t]=cx-1;qy[t++]=cy}
        if(cy+1<=mH&&G[cy+1,cx]==0){G[cy+1,cx]=2;qx[t]=cx;qy[t++]=cy+1}
        if(cy-1>=0&&G[cy-1,cx]==0){G[cy-1,cx]=2;qx[t]=cx;qy[t++]=cy-1}
    }
    for(y=0;y<=mH;y++){
        rs=0
        for(x=0;x<=mW;x++){
            v=(G[y,x]!=2?CW[x]*RH[y]:0);rs+=v
            S[y,x]=rs+(y>0?S[y-1,x]:0)
        }
    }
    maxA=0
    for(i=1;i<=nP;i++){
        for(j=i;j<=nP;j++){
            a=(abs(PX[i]-PX[j])+1)*(abs(PY[i]-PY[j])+1)
            if(a>maxA&&getSum(mX[PX[i]],mY[PY[i]],mX[PX[j]],mY[PY[j]])==a)maxA=a
        }
    }
    print "Largest valid area: " maxA
}

