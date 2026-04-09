
#!/usr/bin/env bash
awk '
BEGIN{
    ARGV[1]="input.txt";ARGC=2
    split("0 1 -1 0 0",dx," ")
    split("0 0 0 1 -1",dy," ")
}
{
    l[NR-1]=$0
}
END{
    H=NR-2;W=length(l[0])-2
    for(i=1;i<=length(l[0]);i++)if(substr(l[0],i,1)==".")SX=i-2
    SY=-1
    for(i=1;i<=length(l[NR-1]);i++)if(substr(l[NR-1],i,1)==".")GX=i-2
    GY=H
    for(y=0;y<H;y++)for(x=0;x<W;x++){
        c=substr(l[y+1],x+2,1)
        if(c==">")g[x,y]=1
        else if(c=="<")g[x,y]=2
        else if(c=="v")g[x,y]=4
        else if(c=="^")g[x,y]=8
    }
    P=lcm(W,H)
    t=solve(SX,SY,GX,GY,0)
    t=solve(GX,GY,SX,SY,t)
    print solve(SX,SY,GX,GY,t)
}
function gcd(a,b){while(b){t=a%b;a=b;b=t}return a}
function lcm(a,b){return (a*b)/gcd(a,b)}
function is_safe(x,y,t,   xl,xr,yu,yd,tw,th){
    if((x==SX&&y==SY)||(x==GX&&y==GY))return 1
    if(x<0||x>=W||y<0||y>=H)return 0
    tw=t%W;th=t%H
    xl=(x-tw+W)%W;xr=(x+tw)%W
    yu=(y-th+H)%H;yd=(y+th)%H
    return !(g[xl,y]==1||g[xr,y]==2||g[x,yu]==4||g[x,yd]==8)
}
function solve(sx,sy,tx,ty,st,   qh,qt,qx,qy,qv,v,cx,cy,ct,nt,mt,i,nx,ny){
    qh=qt=1
    qx[qt]=sx;qy[qt]=sy;qv[qt]=st;qt++
    v[sx,sy,st%P]=1
    while(qh<qt){
        cx=qx[qh];cy=qy[qh];ct=qv[qh];delete qx[qh];delete qy[qh];delete qv[qh];qh++
        nt=ct+1;mt=nt%P
        for(i=1;i<=5;i++){
            nx=cx+dx[i];ny=cy+dy[i]
            if(nx==tx&&ny==ty)return nt
            if(is_safe(nx,ny,nt)&&!((nx,ny,mt) in v)){
                v[nx,ny,mt]=1
                qx[qt]=nx;qy[qt]=ny;qv[qt]=nt;qt++
            }
        }
    }
}
' 
