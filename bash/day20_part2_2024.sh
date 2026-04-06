Solution:
#!/bin/bash
awk '{
  y++;n=split($0,c,"")
  for(x=1;x<=n;x++)if(c[x]!="#"){t[y,x]=1;if(c[x]=="S"){sy=y;sx=x}}
}
END {
  qr[1]=sy;qc[1]=sx;d[sy,sx]=0;nq=1
  split("0 0 1 -1",dy);split("1 -1 0 0",dx)
  for(i=1;i<=nq;i++){
    cy=qr[i];cx=qc[i]
    for(k=1;k<=4;k++){
      ny=cy+dy[k];nx=cx+dx[k]
      if(t[ny,nx] && !((ny,nx) in d)){
        d[ny,nx]=d[cy,cx]+1;qr[++nq]=ny;qc[nq]=nx
      }
    }
  }
  for(i=1;i<=nq;i++){
    y1=qr[i];x1=qc[i];d1=d[y1,x1]
    for(iy=-20;iy<=20;iy++){
      ay=iy<0?-iy:iy
      for(ix=-(20-ay);ix<=(20-ay);ix++){
        y2=y1+iy;x2=x1+ix;ax=ix<0?-ix:ix
        if((y2,x2) in d && d[y2,x2]-d1-(ay+ax)>=100)a++
      }
    }
  }
  print a+0
}' input.txt