
#!/usr/bin/env bash
awk '
{
    lines[NR]=$0
}
END{
    h=NR
    w=length(lines[1])
    for(r=1;r<=h;r++){
        for(c=1;c<=w;c++){
            grid[r,c]=substr(lines[r],c,1)
        }
    }
    for(r=0;r<=h+1;r++)grid[r,0]=grid[r,w+1]="."
    for(c=0;c<=w+1;c++)grid[0,c]=grid[h+1,c]="."
    split("-1 0 1 0 0 -1 0 1",dirs)
    for(sr=1;sr<=h;sr++){
        for(sc=1;sc<=w;sc++){
            if(visited[sr,sc])continue
            plant=grid[sr,sc]
            area=peri=0
            q[1]=sr" "sc; qs=1; qe=2
            visited[sr,sc]=1
            delete coords
            while(qs<qe){
                split(q[qs++],rc," ")
                r=rc[1];c=rc[2]
                area++; coords[area]=r" "c
                for(i=1;i<=8;i+=2){
                    dr=dirs[i];dc=dirs[i+1]
                    nr=r+dr;nc=c+dc
                    if(grid[nr,nc]!=plant)peri++
                    else if(!visited[nr,nc]){
                        visited[nr,nc]=1
                        q[qe++]=nr" "nc
                    }
                }
            }
            P1+=area*peri
            delete inRegion
            for(i=1;i<=area;i++){
                split(coords[i],rc," ")
                inRegion[rc[1],rc[2]]=1
            }
            topF=botF=lefF=rigF=topA=botA=lefA=rigA=0
            for(i=1;i<=area;i++){
                split(coords[i],rc," ")
                r=rc[1];c=rc[2]
                if(grid[r-1,c]!=plant)topF++
                if(grid[r+1,c]!=plant)botF++
                if(grid[r,c-1]!=plant)lefF++
                if(grid[r,c+1]!=plant)rigF++
                if(inRegion[r,c+1]){
                    if(grid[r-1,c]!=plant && grid[r-1,c+1]!=plant)topA++
                    if(grid[r+1,c]!=plant && grid[r+1,c+1]!=plant)botA++
                }
                if(inRegion[r+1,c]){
                    if(grid[r,c-1]!=plant && grid[r+1,c-1]!=plant)lefA++
                    if(grid[r,c+1]!=plant && grid[r+1,c+1]!=plant)rigA++
                }
            }
            sides=(topF-topA)+(botF-botA)+(lefF-lefA)+(rigF-rigA)
            P2+=area*sides
        }
    }
    print P1
    print P2
}
' input.txt
