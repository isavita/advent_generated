
#!/usr/bin/env bash
set -euo pipefail

awk '
BEGIN {
  dx[0]=1;  dr[0]=0
  dx[1]=0;  dr[1]=1
  dx[2]=-1; dr[2]=1
  dx[3]=-1; dr[3]=0
  dx[4]=0;  dr[4]=-1
  dx[5]=1;  dr[5]=-1

  while ((getline line < "input.txt") > 0) {
    q=0; r=0; i=1; n=length(line)
    while (i <= n) {
      ch = substr(line, i, 1)
      if (ch == "e") { q++; i++ }
      else if (ch == "w") { q--; i++ }
      else {
        ch2 = substr(line, i+1, 1)
        if (ch == "n") {
          if (ch2 == "e") { q++; r-- }
          else { r-- }
        } else {
          if (ch2 == "e") { r++ }
          else { q--; r++ }
        }
        i += 2
      }
    }
    k = q "," r
    if (black[k]) delete black[k]
    else black[k] = 1
  }
  close("input.txt")

  for (day=1; day<=100; day++) {
    delete check
    for (k in black) if (black[k]) {
      check[k]=1
      split(k,a,","); q0=a[1]+0; r0=a[2]+0
      for (t=0; t<6; t++) check[(q0+dx[t]) "," (r0+dr[t])] = 1
    }

    delete newblack
    for (k in check) {
      split(k,a,","); q=a[1]+0; r=a[2]+0
      c=0
      for (t=0; t<6; t++) if (black[(q+dx[t]) "," (r+dr[t])]) c++
      if (black[k]) {
        if (c==1 || c==2) newblack[k]=1
      } else {
        if (c==2) newblack[k]=1
      }
    }

    delete black
    for (k in newblack) black[k]=1
  }

  count=0
  for (k in black) if (black[k]) count++
  print count
}
' 
