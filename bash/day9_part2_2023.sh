#!/usr/bin/env bash
set -euo pipefail

solve() {
  awk '
  {
    n=NF
    for(i=1;i<=NF;i++) a[i]=$i
    depth=0
    while(1){
      zero=1
      for(i=1;i<n;i++){
        d[i]=a[i+1]-a[i]
        if(d[i]!=0) zero=0
      }
      depth++
      for(i=1;i<n;i++) hist[depth,i]=d[i]
      if(n==1 || zero) break
      for(i=1;i<n;i++) a[i]=d[i]
      n=n-1
    }
    past=0
    for(k=depth;k>=0;k--){
      if(k==0) v=$1
      else v=hist[k,1]
      past=v-past
    }
    sum+=past
  }
  END{print sum}
  ' input.txt
}

main() {
  solve
}

main "$@"