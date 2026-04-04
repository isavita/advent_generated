
#!/usr/bin/env bash
set -euo pipefail

awk '
function battle(boost,    i,j,k,s1,s2,atk,def,best,maxd,cur,epi,epj,t,changed,killed,st) {
  for (i=1;i<=N;i++) { u[i]=U[i]; d[i]=D[i]+(S[i]==1?boost:0) }
  while (1) {
    s1=s2=0
    for (i=1;i<=N;i++) if (u[i]>0) {
      if (S[i]==1) s1+=u[i]; else s2+=u[i]
    }
    if (s1==0 || s2==0) break

    for (i=1;i<=N;i++) { idx[i]=i; target[i]=0; targeted[i]=0 }

    for (i=1;i<N;i++) for (j=i+1;j<=N;j++) {
      epi=u[idx[i]]*d[idx[i]]
      epj=u[idx[j]]*d[idx[j]]
      if (epj>epi || (epj==epi && I[idx[j]]>I[idx[i]])) {
        t=idx[i]; idx[i]=idx[j]; idx[j]=t
      }
    }

    for (i=1;i<=N;i++) {
      atk=idx[i]
      if (u[atk]<=0) continue
      best=0; maxd=0
      for (def=1;def<=N;def++) {
        if (u[def]<=0 || S[atk]==S[def] || targeted[def]) continue
        cur=u[atk]*d[atk]
        if (imm[def SUBSEP T[atk]]) cur=0
        else if (wek[def SUBSEP T[atk]]) cur*=2
        if (cur==0) continue
        if (cur>maxd) {
          maxd=cur; best=def
        } else if (cur==maxd) {
          epi=u[def]*d[def]
          epj=u[best]*d[best]
          if (epi>epj || (epi==epj && I[def]>I[best])) best=def
        }
      }
      if (best) { target[atk]=best; targeted[best]=1 }
    }

    for (i=1;i<=N;i++) idx[i]=i
    for (i=1;i<N;i++) for (j=i+1;j<=N;j++) if (I[idx[j]]>I[idx[i]]) { t=idx[i]; idx[i]=idx[j]; idx[j]=t }

    changed=0
    for (i=1;i<=N;i++) {
      atk=idx[i]; def=target[atk]
      if (u[atk]>0 && def && u[def]>0) {
        cur=u[atk]*d[atk]
        if (imm[def SUBSEP T[atk]]) cur=0
        else if (wek[def SUBSEP T[atk]]) cur*=2
        killed=int(cur/H[def])
        if (killed>u[def]) killed=u[def]
        if (killed>0) { u[def]-=killed; changed=1 }
      }
    }
    if (!changed) return -1
  }
  return (s1>0)?s1:-2
}

BEGIN {
  while ((getline line < "input.txt") > 0) {
    if (line ~ /Immune System/) { s=1; continue }
    if (line ~ /Infection/) { s=2; continue }
    if (line=="") continue

    N++
    split(line, a, " ")
    S[N]=s
    U[N]=a[1]+0
    H[N]=a[5]+0
    I[N]=a[length(a)]+0
    D[N]=a[length(a)-5]+0
    T[N]=a[length(a)-4]

    if (line ~ /\(/) {
      match(line, /\(.*\)/)
      p=substr(line, RSTART+1, RLENGTH-2)
      m=split(p, parts, "; ")
      for (k=1;k<=m;k++) {
        if (parts[k] ~ /^immune to /) {
          sub(/^immune to /, "", parts[k])
          ni=split(parts[k], items, ", ")
          for (l=1;l<=ni;l++) imm[N SUBSEP items[l]]=1
        } else {
          sub(/^weak to /, "", parts[k])
          ni=split(parts[k], items, ", ")
          for (l=1;l<=ni;l++) wek[N SUBSEP items[l]]=1
        }
      }
    }
  }

  for (boost=0;;boost++) {
    res=battle(boost)
    if (res>=0) { print res; exit }
  }
}
' input.txt
