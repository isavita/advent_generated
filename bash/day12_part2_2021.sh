
#!/bin/bash
awk -F- '{g[$1]=g[$1]" "$2; g[$2]=g[$2]" "$1}
function f(u,v,d,  i,c,n,t,r){
  if(u=="end") return 1
  n=split(g[u],c," ")
  for(i=1;i<=n;i++){
    t=c[i]
    if(t=="start") continue
    if(t~/[a-z]/){
      if(index(v,","t",")==0) r+=f(t,v t",",d)
      else if(!d) r+=f(t,v t",",1)
    } else r+=f(t,v t",",d)
  }
  return r+0
}
END{print f("start",",start,",0)}' input.txt
