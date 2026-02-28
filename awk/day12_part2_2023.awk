#!/usr/bin/awk -f
BEGIN{
  total=0
  while ((getline line < "input.txt") > 0) {
    split(line, parts, " ")
    pattern=parts[1]
    split(parts[2], nums, ",")
    n=length(nums)
    springs=""
    for(k=1;k<=5;k++){
      if(k>1) springs=springs"?"
      springs=springs pattern
    }
    m=0
    for(k=1;k<=5;k++){
      for(i=1;i<=n;i++) groups_unfold[++m]=nums[i]+0
    }
    initRow(springs, groups_unfold, m)
    delete memo
    total+=dfs(1,1,0)
  }
  print total
}
function initRow(sprStr, gArr, gLen,   i){
  L=length(sprStr)
  delete spr
  for(i=1;i<=L;i++) spr[i]=substr(sprStr,i,1)
  G=gLen
  delete grp
  for(i=1;i<=G;i++) grp[i]=gArr[i]
}
function dfs(i, gi, cont,   key,ch,res){
  if (i > L) {
    if (gi > G && cont==0) return 1
    if (gi == G && cont == grp[gi]) return 1
    return 0
  }
  key=i SUBSEP gi SUBSEP cont
  if ((key) in memo) return memo[key]
  res=0
  ch=spr[i]
  if (ch=="." || ch=="?") {
    if (cont==0) res+=dfs(i+1, gi, 0)
    else if (gi <= G && cont==grp[gi]) res+=dfs(i+1, gi+1, 0)
  }
  if (ch=="#" || ch=="?") {
    if (gi <= G && cont < grp[gi]) res+=dfs(i+1, gi, cont+1)
  }
  memo[key]=res
  return res
}