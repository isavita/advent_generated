
#!/usr/bin/env bash
set -euo pipefail

awk '
BEGIN{
    while((getline line < "input.txt")>0){
        rows++
        cols=split(line,a,"")
        for(c=1;c<=cols;c++) g[rows,c]=a[c]
    }

    dr[0]=-1; dc[0]=0
    dr[1]=0;  dc[1]=1
    dr[2]=1;  dc[2]=0
    dr[3]=0;  dc[3]=-1

    heapSize=0
    push("1,1,0,0",0)
    dist["1,1,0,0"]=0

    while(heapSize>0){
        cur=pop()
        dcur=dist[cur]
        split(cur,x,",")
        r=x[1]+0; c=x[2]+0; dir=x[3]+0; run=x[4]+0

        if(r==rows && c==cols){
            print dcur
            exit
        }

        for(nd=0; nd<4; nd++){
            nr=r+dr[nd]
            nc=c+dc[nd]
            if(nr<1 || nr>rows || nc<1 || nc>cols) continue
            if(nd==dir) nrun=run+1
            else nrun=1
            if(nrun>3) continue
            if((nd-dir==2) || (dir-nd==2)) continue

            ndist=dcur + (g[nr,nc]+0)
            key=nr "," nc "," nd "," nrun
            if(!(key in dist) || ndist<dist[key]){
                dist[key]=ndist
                push(key,ndist)
            }
        }
    }

    print "No path"
}

function push(k,d,   i,p,tk,td){
    heapSize++
    heapKey[heapSize]=k
    heapDist[heapSize]=d
    i=heapSize
    while(i>1){
        p=int(i/2)
        if(heapDist[p] <= heapDist[i]) break
        tk=heapKey[p]; heapKey[p]=heapKey[i]; heapKey[i]=tk
        td=heapDist[p]; heapDist[p]=heapDist[i]; heapDist[i]=td
        i=p
    }
}

function pop(   k,i,l,r,p,tk,td){
    k=heapKey[1]
    heapKey[1]=heapKey[heapSize]
    heapDist[1]=heapDist[heapSize]
    heapSize--
    i=1
    while(1){
        l=i*2
        r=l+1
        if(l>heapSize) break
        p=l
        if(r<=heapSize && heapDist[r] < heapDist[l]) p=r
        if(heapDist[i] <= heapDist[p]) break
        tk=heapKey[p]; heapKey[p]=heapKey[i]; heapKey[i]=tk
        td=heapDist[p]; heapDist[p]=heapDist[i]; heapDist[i]=td
        i=p
    }
    return k
}
' 
