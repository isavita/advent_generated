
#!/usr/bin/env bash
awk '
BEGIN{
    ARGV[1]="input.txt"
}
{
    line=$0
    sub(/\)/,"",line)
    split_idx=index(line," (contains ")
    if(split_idx==0){
        ing_str=line
        n_alg=0
    }else{
        ing_str=substr(line,1,split_idx-1)
        alg_str=substr(line,split_idx+11)
        gsub(/,/, "", alg_str)
        n_alg=split(alg_str,algs," ")
    }
    n_ing=split(ing_str,ings," ")
    delete curr
    for(i=1;i<=n_ing;i++){
        ing=ings[i]
        total[ing]++
        all_ing[ing]=1
        curr[ing]=1
    }
    for(i=1;i<=n_alg;i++){
        a=algs[i]
        all_alg[a]=1
        if(!(a in seen)){
            seen[a]=1
            for(ing in curr) potential[a,ing]=1
        }else{
            for(ing in all_ing) if(((a,ing) in potential)&&!(ing in curr)) delete potential[a,ing]
        }
    }
}
END{
    sum=0
    for(ing in all_ing){
        poss=0
        for(a in all_alg) if((a,ing) in potential){poss=1;break}
        if(!poss) sum+=total[ing]
    }
    print sum
    cnt=0
    for(a in all_alg) cnt++
    resolved=0
    while(resolved<cnt){
        for(a in all_alg){
            if(a in final) continue
            opt=0;cand=""
            for(ing in all_ing) if((a,ing) in potential){opt++;cand=ing}
            if(opt==1){
                final[a]=cand
                resolved++
                for(b in all_alg) if(b!=a) delete potential[b,cand]
            }
        }
    }
    n=0
    for(a in final) sorted[++n]=a
    for(i=1;i<=n;i++) for(j=i+1;j<=n;j++) if(sorted[i]>sorted[j]){tmp=sorted[i];sorted[i]=sorted[j];sorted[j]=tmp}
    for(i=1;i<=n;i++) printf "%s%s", final[sorted[i]], (i==n?"":",")
    printf "\n"
}
' input.txt
