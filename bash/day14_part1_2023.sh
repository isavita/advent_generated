
#!/usr/bin/env bash
awk '
function calc(  x,y,l){l=0;for(x=1;x<=w;x++)for(y=1;y<=h;y++)if(a[x,y]=="O")l+=h-y+1;print l;exit}
{
  h++
  w=length($0)
  for(i=1;i<=w;i++){
    c=substr($0,i,1)
    a[i,h]=c
  }
}
END{
  for(x=1;x<=w;x++){
    t=1
    for(y=1;y<=h;y++){
      c=a[x,y]
      if(c=="#") t=y+1
      else if(c=="O"){
        if(y!=t){a[x,t]="O";a[x,y]="."}
        t++
      }
    }
  }
  l=0
  for(x=1;x<=w;x++)for(y=1;y<=h;y++)if(a[x,y]=="O")l+=h-y+1
  print l
}
' input.txt
