#!/usr/bin/awk -f
BEGIN{
    for(i=0;i<10;i++){x[i]=0;y[i]=0}
    visited["0,0"]=1
    while((getline line < "input.txt")>0){
        split(line,a," ")
        dir=a[1]; steps=a[2]+0
        for(s=0;s<steps;s++){
            if(dir=="R") x[0]++
            else if(dir=="L") x[0]--
            else if(dir=="U") y[0]++
            else if(dir=="D") y[0]--
            for(i=1;i<10;i++){
                dx=x[i-1]-x[i]; dy=y[i-1]-y[i]
                if(dx>1||dx<-1||dy>1||dy<-1){
                    if(dx>0) x[i]++; else if(dx<0) x[i]--
                    if(dy>0) y[i]++; else if(dy<0) y[i]--
                }
            }
            visited[x[9]","y[9]]=1
        }
    }
    close("input.txt")
}
END{
    cnt=0
    for(k in visited) cnt++
    print cnt
}