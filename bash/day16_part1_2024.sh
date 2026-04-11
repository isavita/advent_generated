#!/usr/bin/env bash
awk '
BEGIN{
    rows=0
    while((getline line<"input.txt")>0){
        cols=length(line)
        for(c=1;c<=cols;c++){
            char=substr(line,c,1)
            grid[rows","c-1]=char
            if(char=="S"){start_r=rows;start_c=c-1}
            if(char=="E"){end_r=rows;end_c=c-1}
        }
        rows++
    }
    dr[0]=0;dc[0]=1
    dr[1]=1;dc[1]=0
    dr[2]=0;dc[2]=-1
    dr[3]=-1;dc[3]=0
    start_key=start_r","start_c","0
    pq[start_key]=0
    while(length(pq)>0){
        min_cost=-1;min_key=""
        for(k in pq){
            if(min_key==""||pq[k]<min_cost){
                min_cost=pq[k];min_key=k
            }
        }
        split(min_key,s,",");r=s[1];c=s[2];dir=s[3]
        delete pq[min_key]
        if(min_key in visited){next}
        visited[min_key]=1
        if(r==end_r&&c==end_c){print min_cost;exit}
        nr=r+dr[dir];nc=c+dc[dir]
        if(nr>=0&&nr<rows&&nc>=0&&nc<cols&&grid[nr","nc]!="#"){
            nk=nr","nc","dir;ncost=min_cost+1
            if(!(nk in visited)){
                if(!(nk in pq)||ncost<pq[nk]){pq[nk]=ncost}
            }
        }
        nd=(dir+1)%4;nk=r","c","nd;ncost=min_cost+1000
        if(!(nk in visited)){
            if(!(nk in pq)||ncost<pq[nk]){pq[nk]=ncost}
        }
        nd=(dir-1+4)%4;nk=r","c","nd;ncost=min_cost+1000
        if(!(nk in visited)){
            if(!(nk in pq)||ncost<pq[nk]){pq[nk]=ncost}
        }
    }
}
' input.txt
