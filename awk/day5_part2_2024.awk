
BEGIN{
    isU=0; rc=0; sum=0
    while((getline l<"input.txt")>0){
        sub(/\r/,"",l)
        if(l==""){isU=1; continue}
        if(!isU){
            split(l,p,"|")
            if(length(p)!=2)continue
            rulesX[++rc]=+p[1]; rulesY[rc]=+p[2]
        }else{
            n=split(l,a,",")
            delete pres; delete pos
            for(i=1;i<=n;i++){gsub(/^ +| +$/,"",a[i]); v=+a[i]; upd[i]=v; pres[v]=1; pos[v]=i}
            ok=1
            for(i=1;i<=rc;i++)if(pres[rulesX[i]]&&pres[rulesY[i]]&&pos[rulesX[i]]>=pos[rulesY[i]]){ok=0;break}
            if(ok)continue
            delete indeg; delete adj
            for(i=1;i<=n;i++){v=upd[i]; indeg[v]=0; adj[v]=""}
            for(i=1;i<=rc;i++)if(pres[rulesX[i]]&&pres[rulesY[i]]){
                indeg[rulesY[i]]++; adj[rulesX[i]]=adj[rulesX[i]]?(adj[rulesX[i]]" "rulesY[i]):rulesY[i]
            }
            qh=1; qt=0
            for(i=1;i<=n;i++)if(indeg[upd[i]]==0)queue[++qt]=upd[i]
            sc=0
            while(qh<=qt){
                v=queue[qh++]; sorted[++sc]=v
                split(adj[v],nb," ")
                for(j=1;j<=length(nb);j++)if(nb[j]!=""){
                    w=nb[j]; indeg[w]--
                    if(indeg[w]==0)queue[++qt]=w
                }
            }
            mid=int(sc/2)+1
            sum+=sorted[mid]
        }
    }
    print sum
}
