#!/usr/bin/env bash
set -euo pipefail

awk '
function rank_category(s,   i,cnt,val,ch) {
  split("", cnt)
  for (i=1;i<=length(s);i++) {
    ch=substr(s,i,1)
    cnt[ch]++
  }
  val=1
  for (ch in cnt) val*=cnt[ch]
  if (val==1) return 6
  else if (val==2) return 5
  else if (val==3) return 3
  else if (val==4) { if (length(cnt)==2) return 1; else return 4 }
  else if (val==5) return 0
  else if (val==6) return 2
  return 6
}
function hexval(s,   i,ch,acc,v) {
  acc=0
  for (i=1;i<=length(s);i++) {
    ch=substr(s,i,1)
    if (ch=="A") ch="E"
    else if (ch=="T") ch="A"
    else if (ch=="J") ch="B"
    else if (ch=="Q") ch="C"
    else if (ch=="K") ch="D"
    if (ch>="0" && ch<="9") v=ch+0
    else v=10 + (index("ABCDEF", ch)-1)
    acc = acc*16 + v
  }
  return acc
}
BEGIN {
  n=0
}
NF==2 {
  cards[++n]=$1
  bid[n]=$2+0
  cat[n]=rank_category($1)
  rankv[n]=hexval($1)
}
END {
  for (c=0;c<7;c++) idx[c]=0
  for (i=1;i<=n;i++) {
    c=cat[i]
    idx[c]++
    bycat[c,idx[c]]=i
  }
  outn=0
  for (c=0;c<7;c++) {
    m=idx[c]
    if (m==0) continue
    for (i=1;i<=m;i++) order[i]=i
    # sort descending by rankv within category
    for (i=1;i<=m;i++) {
      for (j=i+1;j<=m;j++) {
        a=bycat[c,order[i]]; b=bycat[c,order[j]]
        if (rankv[a] < rankv[b]) { t=order[i]; order[i]=order[j]; order[j]=t }
      }
    }
    for (i=1;i<=m;i++) {
      out[++outn]=bycat[c,order[i]]
    }
  }
  total=0
  for (i=1;i<=outn;i++) {
    idxh=out[i]
    total += bid[idxh]*(outn - i + 1)
  }
  print total
}
' input.txt