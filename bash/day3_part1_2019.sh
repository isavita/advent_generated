
#!/usr/bin/env bash
awk '
BEGIN { FS="," }
NR==1 {
  x=0; y=0
  for (i=1; i<=NF; i++) {
    dir=substr($i,1,1); steps=substr($i,2)+0
    for (s=0; s<steps; s++) {
      if (dir=="U") y++
      else if (dir=="D") y--
      else if (dir=="L") x--
      else if (dir=="R") x++
      vis[x "," y]=1
    }
  }
}
NR==2 {
  x=0; y=0; min=2147483647
  for (i=1; i<=NF; i++) {
    dir=substr($i,1,1); steps=substr($i,2)+0
    for (s=0; s<steps; s++) {
      if (dir=="U") y++
      else if (dir=="D") y--
      else if (dir=="L") x--
      else if (dir=="R") x++
      k=x "," y
      if (vis[k]) {
        d=(x<0?-x:x)+(y<0?-y:y)
        if (d<min) min=d
      }
    }
  }
  print min
}
' input.txt
