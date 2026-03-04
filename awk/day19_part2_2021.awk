
#!/usr/bin/awk -f
function abs(v){return v<0?-v:v}
function set_rot(x,y,z,r){
if(r==0){tx=x;ty=y;tz=z}else if(r==1){tx=x;ty=z;tz=-y}else if(r==2){tx=x;ty=-y;tz=-z}else if(r==3){tx=x;ty=-z;tz=y}else if(r==4){tx=-x;ty=-y;tz=z}else if(r==5){tx=-x;ty=z;tz=y}else if(r==6){tx=-x;ty=y;tz=-z}else if(r==7){tx=-x;ty=-z;tz=-y}else if(r==8){tx=y;ty=x;tz=-z}else if(r==9){tx=y;ty=z;tz=x}else if(r==10){tx=y;ty=-x;tz=z}else if(r==11){tx=y;ty=-z;tz=-x}else if(r==12){tx=-y;ty=-x;tz=-z}else if(r==13){tx=-y;ty=z;tz=-x}else if(r==14){tx=-y;ty=x;tz=z}else if(r==15){tx=-y;ty=-z;tz=x}else if(r==16){tx=z;ty=x;tz=y}else if(r==17){tx=z;ty=y;tz=-x}else if(r==18){tx=z;ty=-x;tz=-y}else if(r==19){tx=z;ty=-y;tz=x}else if(r==20){tx=-z;ty=-x;tz=y}else if(r==21){tx=-z;ty=y;tz=x}else if(r==22){tx=-z;ty=x;tz=-y}else if(r==23){tx=-z;ty=-y;tz=-x}
}
BEGIN{FS=",";ARGV[1]="input.txt";ARGC=2;s=-1}
/^--- scanner/{s++;b=0;next}
NF==3{sc[s,b,0]=$1;sc[s,b,1]=$2;sc[s,b,2]=$3;bc[s]=++b}
END{
n=s+1;for(i=1;i<n;i++)u[i]=i;sx[0]=sy[0]=sz[0]=0;q[0]=0;ql=1;qi=0
for(b=0;b<bc[0];b++){sa[0,b,0]=sc[0,b,0];sa[0,b,1]=sc[0,b,1];sa[0,b,2]=sc[0,b,2]}
while(qi<ql){
i=q[qi++];for(j in u){
f=0;for(r=0;r<24;r++){
delete dt;for(bj=0;bj<bc[j];bj++){set_rot(sc[j,bj,0],sc[j,bj,1],sc[j,bj,2],r);rx[bj]=tx;ry[bj]=ty;rz[bj]=tz}
for(bi=0;bi<bc[i];bi++){
ax=sa[i,bi,0];ay=sa[i,bi,1];az=sa[i,bi,2]
for(bj=0;bj<bc[j];bj++){
dx=ax-rx[bj];dy=ay-ry[bj];dz=az-rz[bj]
if(++dt[dx,dy,dz]==12){
sx[j]=dx;sy[j]=dy;sz[j]=dz;q[ql++]=j;delete u[j];f=1
for(bk=0;bk<bc[j];bk++){sa[j,bk,0]=rx[bk]+dx;sa[j,bk,1]=ry[bk]+dy;sa[j,bk,2]=rz[bk]+dz}
break
}}if(f)break}if(f)break}}}
m=0;for(i=0;i<n;i++)for(j=i+1;j<n;j++){
d=abs(sx[i]-sx[j])+abs(sy[i]-sy[j])+abs(sz[i]-sz[j])
if(d>m)m=d
}
print m
}
