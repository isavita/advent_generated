
#!/usr/bin/awk -f
BEGIN{
    file="input.txt"
    while ((getline l<file)>0){
        ++n; w[n]=l+0; total+=w[n]
    }
    close(file)
    if (total%4){print "Cannot balance the sleight, total weight is not divisible by 4."; exit}
    target=total/4; bestCount=1e9; bestProd=0
    backtrack(1,0,0,1)
    print bestProd
}
function backtrack(i,sum,cnt,prod,   j){
    if (sum==target){
        if (cnt<bestCount || (cnt==bestCount && prod<bestProd)){
            bestCount=cnt; bestProd=prod
        }
        return
    }
    if (sum>target) return
    for (j=i;j<=n;j++) backtrack(j+1,sum+w[j],cnt+1,prod*w[j])
}
