
#!/usr/bin/env bash
awk '
BEGIN {
  while ((getline < "input.txt") > 0) {
    gsub(/x=|y=|z=|,|\.\./, " ")
    v1=$2; v2=$3; v3=$4; v4=$5; v5=$6; v6=$7
    n=0
    for (i=1; i<=c; i++) {
      x1=(v1>cx1[i]?v1:cx1[i]); x2=(v2<cx2[i]?v2:cx2[i])
      y1=(v3>cy1[i]?v3:cy1[i]); y2=(v4<cy2[i]?v4:cy2[i])
      z1=(v5>cz1[i]?v5:cz1[i]); z2=(v6<cz2[i]?v6:cz2[i])
      if (x1<=x2 && y1<=y2 && z1<=z2) {
        n++; t1[n]=x1; t2[n]=x2; t3[n]=y1; t4[n]=y2; t5[n]=z1; t6[n]=z2; ts[n]=-cs[i]
      }
    }
    for (i=1; i<=n; i++) {
      c++; cx1[c]=t1[i]; cx2[c]=t2[i]; cy1[c]=t3[i]; cy2[c]=t4[i]; cz1[c]=t5[i]; cz2[c]=t6[i]; cs[c]=ts[i]
    }
    if ($1=="on") {
      c++; cx1[c]=v1; cx2[c]=v2; cy1[c]=v3; cy2[c]=v4; cz1[c]=v5; cz2[c]=v6; cs[c]=1
    }
  }
  for (i=1; i<=c; i++)
    r += cs[i]*(cx2[i]-cx1[i]+1)*(cy2[i]-cy1[i]+1)*(cz2[i]-cz1[i]+1)
  printf "%.0f\n", r
}
' /dev/null
