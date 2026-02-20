
#!/usr/bin/awk -f
BEGIN{
    FS=","
    # read grid
    rows=0
    while((getline line < "input.txt")>0){
        rows++
        cols=split(line,arr,"")
        for(c=1;c<=cols;c++) grid[rows,c]=arr[c]
    }
    endR=rows; endC=cols

    # directions: 0=up,1=right,2=down,3=left
    dr[0]=-1; dr[1]=0; dr[2]=1; dr[3]=0
    dc[0]=0;  dc[1]=1; dc[2]=0; dc[3]=-1

    # priority queue
    heapSize=0
    heapPush("1,1,0,0",0)

    startKey="1,1,0,0"
    dist[startKey]=0

    while(heapSize>0){
        curKey=heapPop()
        curDist=dist[curKey]
        split(curKey,arr,",")
        r=arr[1]+0; c=arr[2]+0; d=arr[3]+0; s=arr[4]+0

        if(r==endR && c==endC){print curDist; exit}

        for(newD=0;newD<4;newD++){
            newS=(newD==d? s+1:1)
            if(newS>3) continue
            if(abs(newD-d)==2) continue
            nr=r+dr[newD]; nc=c+dc[newD]
            if(nr<1||nr>rows||nc<1||nc>cols) continue
            cost=grid[nr,nc]+0
            newDist=curDist+cost
            newKey=nr "," nc "," newD "," newS
            if(!(newKey in dist) || newDist<dist[newKey]){
                dist[newKey]=newDist
                heapPush(newKey,newDist)
            }
        }
    }
    print "No path"
}

function heapPush(key,d,   i,p,t){
    heapSize++; heapKey[heapSize]=key; heapDist[heapSize]=d; i=heapSize
    while(i>1){
        p=int(i/2)
        if(heapDist[p]<=heapDist[i]) break
        t=heapKey[p]; heapKey[p]=heapKey[i]; heapKey[i]=t
        t=heapDist[p]; heapDist[p]=heapDist[i]; heapDist[i]=t
        i=p
    }
}
function heapPop(   key,i,l,r,p,t){
    key=heapKey[1]
    heapKey[1]=heapKey[heapSize]
    heapDist[1]=heapDist[heapSize]
    heapSize--
    i=1
    while(1){
        l=i*2; r=l+1
        if(l>heapSize) break
        p=l
        if(r<=heapSize && heapDist[r]<heapDist[l]) p=r
        if(heapDist[i]<=heapDist[p]) break
        t=heapKey[p]; heapKey[p]=heapKey[i]; heapKey[i]=t
        t=heapDist[p]; heapDist[p]=heapDist[i]; heapDist[i]=t
        i=p
    }
    return key
}
function abs(x){return x<0?-x:x}
