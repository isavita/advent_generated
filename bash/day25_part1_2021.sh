
#!/bin/bash
awk '{
  w=length; for(i=0;i<w;i++) g[h*w+i]=substr($0,i+1,1); h++
}
END {
  while(++s) {
    m=0
    for(i=0;i<h;i++) for(j=0;j<w;j++) {
      k=i*w+j; nk=i*w+(j+1)%w
      if(g[k]==">" && g[nk]==".") { f[++m]=k; t[m]=nk }
    }
    for(i=1;i<=m;i++) { g[f[i]]="."; g[t[i]]=">" }
    e=m; m=0
    for(i=0;i<h;i++) for(j=0;j<w;j++) {
      k=i*w+j; nk=((i+1)%h)*w+j
      if(g[k]=="v" && g[nk]==".") { f[++m]=k; t[m]=nk }
    }
    for(i=1;i<=m;i++) { g[f[i]]="."; g[t[i]]="v" }
    if(e+m==0) break
  }
  print s
}' input.txt
