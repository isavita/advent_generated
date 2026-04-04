
#!/usr/bin/env bash
awk '
function keynorm(n,   i,j,t,minr,minc,key) {
    minr=10^9; minc=10^9
    for(i=0;i<n;i++){ if(tr[i]<minr) minr=tr[i]; if(tc[i]<minc) minc=tc[i] }
    for(i=0;i<n;i++){ tr[i]-=minr; tc[i]-=minc }
    for(i=0;i<n-1;i++) for(j=i+1;j<n;j++) if(tr[i]>tr[j] || (tr[i]==tr[j] && tc[i]>tc[j])) {
        t=tr[i]; tr[i]=tr[j]; tr[j]=t
        t=tc[i]; tc[i]=tc[j]; tc[j]=t
    }
    key=""
    for(i=0;i<n;i++) key=key tr[i] "," tc[i] " "
    return key
}

function add_orient(sid,n,   k,o,i) {
    k=keynorm(n)
    if(uniq[sid SUBSEP k]) return
    uniq[sid SUBSEP k]=1
    o=ocnt[sid]++
    mr[sid SUBSEP o]=0; mc[sid SUBSEP o]=0
    for(i=0;i<n;i++){
        sr[sid SUBSEP o SUBSEP i]=tr[i]
        sc[sid SUBSEP o SUBSEP i]=tc[i]
        if(tr[i]>mr[sid SUBSEP o]) mr[sid SUBSEP o]=tr[i]
        if(tc[i]>mc[sid SUBSEP o]) mc[sid SUBSEP o]=tc[i]
    }
}

function gen(sid,n,   i,j,t) {
    for(i=0;i<n;i++){ tr[i]=or[sid SUBSEP i]; tc[i]=oc[sid SUBSEP i] }
    for(j=0;j<4;j++){
        for(i=0;i<n;i++){ t=tr[i]; tr[i]=tc[i]; tc[i]=-t }
        add_orient(sid,n)
    }
    for(i=0;i<n;i++){ tr[i]=or[sid SUBSEP i]; tc[i]=-oc[sid SUBSEP i] }
    for(j=0;j<4;j++){
        for(i=0;i<n;i++){ t=tr[i]; tr[i]=tc[i]; tc[i]=-t }
        add_orient(sid,n)
    }
}

function solve(p, spos, sor,   id,start,o0,pos,r,c,o,i,ok,off) {
    if(p>reqn) return 1
    if(curarea + rem[p] > maxarea) return 0
    id=req[p]
    if(id==req[p-1]) { start=spos; o0=sor } else { start=0; o0=0 }
    for(pos=start; pos<cells; pos++){
        r=int(pos/w); c=pos%w
        for(o=(pos==start?o0:0); o<ocnt[id]; o++){
            if(r+mr[id SUBSEP o] < h && c+mc[id SUBSEP o] < w){
                ok=1
                for(i=0;i<pcnt[id];i++){
                    off=pos + shoff[id SUBSEP o SUBSEP i]
                    if(grid[off]){ ok=0; break }
                }
                if(ok){
                    for(i=0;i<pcnt[id];i++) grid[pos + shoff[id SUBSEP o SUBSEP i]]=1
                    curarea += pcnt[id]
                    if(solve(p+1,pos,o)) return 1
                    for(i=0;i<pcnt[id];i++) grid[pos + shoff[id SUBSEP o SUBSEP i]]=0
                    curarea -= pcnt[id]
                }
            }
        }
    }
    return 0
}

BEGIN {
    while((getline line < "input.txt") > 0){
        if(line ~ /^[0-9]+:$/){
            sid=substr(line,1,length(line)-1)+0
            if(sid+1>shapes) shapes=sid+1
            row=0
        } else if(line ~ /^[#.]+$/ && sid>=0){
            for(i=1;i<=length(line);i++) if(substr(line,i,1)=="#"){
                p=pcnt[sid]++
                or[sid SUBSEP p]=row
                oc[sid SUBSEP p]=i-1
            }
            row++
        } else if(line ~ /^[0-9]+x[0-9]+:/){
            if(sid>=0){
                for(s=0;s<shapes;s++) gen(s,pcnt[s])
                sid=-1
            }
            split(line,a,":")
            split(a[1],d,"x")
            w=d[1]+0; h=d[2]+0
            cells=w*h; maxarea=cells
            gsub(/^[ ]+/,"",a[2])
            n=split(a[2],cnt,/ +/)
            reqn=0; tot=0
            for(i=1;i<=n;i++){
                for(j=0;j<cnt[i];j++){
                    req[++reqn]=i-1
                    tot += pcnt[i-1]
                }
            }
            if(tot<=maxarea){
                for(i=1;i<reqn;i++) for(j=i+1;j<=reqn;j++) if(pcnt[req[i]] < pcnt[req[j]]) { t=req[i]; req[i]=req[j]; req[j]=t }
                rem[reqn+1]=0
                for(i=reqn;i>=1;i--) rem[i]=rem[i+1]+pcnt[req[i]]
                for(i=1;i<=reqn;i++){
                    id=req[i]
                    for(o=0;o<ocnt[id];o++) for(p=0;p<pcnt[id];p++)
                        shoff[id SUBSEP o SUBSEP p]=sr[id SUBSEP o SUBSEP p]*w + sc[id SUBSEP o SUBSEP p]
                }
                delete grid
                curarea=0
                req[0]=-1
                if(solve(1,0,0)) ans++
            }
        }
    }
    print ans+0
}
' /dev/stdin
