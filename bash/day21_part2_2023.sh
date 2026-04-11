
#!/usr/bin/env bash

awk '
function abs(x){return x<0?-x:x}
BEGIN{
    maxSize=0; numIterations=26501365
    while((getline<"input.txt")>0){
        ++maxSize
        line=$0
        for(x=1;x<=length(line);x++){
            c=substr(line,x,1)
            if(c!="#") garden[x,maxSize]=1
            if(c=="S"){sx=x; sy=maxSize}
        }
    }
    close("input.txt")
    queue[sx,sy]=1
    for(i=0;i<3*maxSize;i++){
        if((i%maxSize)==int((maxSize-1)/2)) done[length(done)+1]=length(queue)
        if(length(done)==3) break
        delete newQueue
        for(p in queue){
            split(p,co,SUBSEP); x=co[1]; y=co[2]
            for(dx=-1;dx<=1;dx++) for(dy=-1;dy<=1;dy++){
                if(abs(dx)==abs(dy)) continue
                nx=x+dx; ny=y+dy
                mx=(nx+10*maxSize)%maxSize; if(mx==0) mx=maxSize
                my=(ny+10*maxSize)%maxSize; if(my==0) my=maxSize
                if(garden[mx,my]) newQueue[nx,ny]=1
            }
        }
        delete queue
        for(p in newQueue) queue[p]=1
    }
    n=int(numIterations/maxSize)
    a=done[1]; b=done[2]; c=done[3]
    sum=a+n*(b-a+((n-1)*(c-2*b+a)/2))
    print sum
}
' 
