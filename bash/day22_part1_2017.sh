
#!/usr/bin/env bash
awk '
BEGIN {
  dx[0]=0; dy[0]=-1
  dx[1]=1; dy[1]=0
  dx[2]=0; dy[2]=1
  dx[3]=-1; dy[3]=0

  y=0
  while ((getline line < "input.txt") > 0) {
    len=length(line)
    for (i=1; i<=len; i++) {
      if (substr(line,i,1) == "#") infected[(i-1) "," y]=1
    }
    width=len
    y++
  }
  close("input.txt")

  height=y
  x=int(width/2)
  yy=int(height/2)
  dir=0
  infections=0

  for (step=0; step<10000; step++) {
    key=x "," yy
    if (key in infected) {
      dir=(dir+1)%4
      delete infected[key]
    } else {
      dir=(dir+3)%4
      infected[key]=1
      infections++
    }
    x+=dx[dir]
    yy+=dy[dir]
  }

  print infections
}
' /dev/null
