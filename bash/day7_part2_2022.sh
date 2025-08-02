#!/usr/bin/env bash
set -euo pipefail

awk '
BEGIN{
  curr_n=1; curr[1]=""; path="";
}
function join_curr(n,   i,s){ s=""; for(i=1;i<=n;i++){ if(i>1)s=s"/"; s=s curr[i]; } return s }
function add_dir(p){ if(!(p in dirs)) dirs[p]=0 }
function add_file(p,s){ files[p]=s }
{
  n=split($0,a," ");
  if(a[1]=="$" && a[2]=="cd"){
    if(a[3]=="/"){ curr_n=1; curr[1]="" }
    else if(a[3]==".."){ if(curr_n>1) curr_n-- }
    else { curr[++curr_n]=a[3] }
    p=join_curr(curr_n); add_dir(p)
  } else if(a[1]!="dir"){
    p=join_curr(curr_n); add_file((p==""?a[2]:p"/"a[2]), a[1]+0)
  }
}
END{
  for(f in files){
    split(f,pp,"/");
    path="";
    for(i=1;i<length(pp);i++){
      if(i==1) path=pp[1];
      else path=path"/"pp[i];
      dirs[path]+=files[f];
    }
  }
  n=0;
  for(d in dirs){ n++; sizes[n]=dirs[d] }
  # sort sizes ascending (shell sort)
  for(g=int(n/2); g>0; g=int(g/2))
    for(i=g+1;i<=n;i++){
      v=sizes[i]; j=i;
      while(j>g && sizes[j-g]>v){ sizes[j]=sizes[j-g]; j-=g }
      sizes[j]=v
    }

  total=70000000; want=30000000;
  avail=total - (("" in dirs)?dirs[""]:0);
  need=want - avail;
  for(i=1;i<=n;i++) if(sizes[i]>=need){ print sizes[i]; exit }
}
' input.txt