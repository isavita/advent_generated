
awk '
BEGIN { dy[1]=-1; dx[1]=0; dy[2]=0; dx[2]=-1; dy[3]=0; dx[3]=1; dy[4]=1; dx[4]=0 }
{ h=NR; w=length($0); for(i=1;i<=w;i++) g0[NR,i]=substr($0,i,1) }
function bfs(sy,sx,ds,qy,qx,  h,t,cy,cx,d,b,ny,nx) {
    delete ds; delete qy; delete qx; h=t=1; qy[1]=sy; qx[1]=sx; ds[sy,sx]=0
    while(h<=t) {
        cy=qy[h]; cx=qx[h++]; d=ds[cy,cx]
        for(b=1; b<=4; b++) {
            ny=cy+dy[b]; nx=cx+dx[b]
            if(g[ny,nx]=="." && !((ny,nx) in ds)) { ds[ny,nx]=d+1; qy[++t]=ny; qx[t]=nx }
        }
    }
}
function sim(ep,  i,j,n,r,u,te,ir,v,b,ny,nx,bd,bty,btx,dist,my,mx,md,mhp,tu,thp,nu,et,qy,qx,qy2,qx2,ord,du,dt,y,x,tmp) {
    nu=0; delete ua; delete g; delete uy; delete ux; delete uh; delete ut; delete up
    for(y=1; y<=h; y++) for(x=1; x<=w; x++) {
        g[y,x]=g0[y,x]
        if(g[y,x]~/[EG]/) {
            nu++; uy[nu]=y; ux[nu]=x; uh[nu]=200; ut[nu]=g[y,x]; up[nu]=(ut[nu]=="E"?ep:3); ua[y,x]=nu
        }
    }
    for(r=0; ; r++) {
        n=0; delete ord
        for(i=1; i<=nu; i++) if(uh[i]>0) ord[++n]=i
        for(i=1; i<n; i++) for(j=i+1; j<=n; j++)
            if(uy[ord[i]]>uy[ord[j]] || (uy[ord[i]]==uy[ord[j]] && ux[ord[i]]>ux[ord[j]])) {
                tmp=ord[i]; ord[i]=ord[j]; ord[j]=tmp
            }
        for(i=1; i<=n; i++) {
            u=ord[i]; if(uh[u]<=0) continue
            et=(ut[u]=="E"?"G":"E"); te=0
            for(v=1; v<=nu; v++) if(uh[v]>0 && ut[v]==et) { te=1; break }
            if(!te) {
                thp=0; for(v=1; v<=nu; v++) if(uh[v]>0) thp+=uh[v]
                return r*thp
            }
            ir=0; for(b=1; b<=4; b++) if(g[uy[u]+dy[b],ux[u]+dx[b]]==et) ir=1
            if(!ir) {
                bfs(uy[u], ux[u], du, qy, qx); bd=1e9; bty=0
                for(v=1; v<=nu; v++) if(uh[v]>0 && ut[v]==et)
                    for(b=1; b<=4; b++) {
                        ny=uy[v]+dy[b]; nx=ux[v]+dx[b]
                        if(g[ny,nx]=="." && (ny,nx) in du) {
                            dist=du[ny,nx]
                            if(!bty || dist<bd || (dist==bd && (ny<bty || (ny==bty && nx<btx)))) {
                                bd=dist; bty=ny; btx=nx
                            }
                        }
                    }
                if(bty) {
                    bfs(bty, btx, dt, qy2, qx2); md=1e9; my=0
                    for(b=1; b<=4; b++) {
                        ny=uy[u]+dy[b]; nx=ux[u]+dx[b]
                        if(g[ny,nx]=="." && (ny,nx) in dt)
                            if(dt[ny,nx]<md) { md=dt[ny,nx]; my=ny; mx=nx }
                    }
                    if(my) {
                        delete ua[uy[u],ux[u]]; g[uy[u],ux[u]]="."; uy[u]=my; ux[u]=mx; g[my,mx]=ut[u]; ua[my,mx]=u
                    }
                }
            }
            tu=0; mhp=1e9
            for(b=1; b<=4; b++) {
                ny=uy[u]+dy[b]; nx=ux[u]+dx[b]
                if(g[ny,nx]==et) {
                    v=ua[ny,nx]; if(!tu || uh[v]<mhp) { mhp=uh[v]; tu=v }
                }
            }
            if(tu) {
                uh[tu]-=up[u]
                if(uh[tu]<=0) {
                    if(ut[tu]=="E" && ep>3) return -1
                    g[uy[tu],ux[tu]]="."; delete ua[uy[tu],ux[tu]]
                }
            }
        }
    }
}
END { for(p=4; ; p++) { res=sim(p); if(res!=-1) { print res; exit } } }
' input.txt
