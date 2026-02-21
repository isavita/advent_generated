
#!/usr/bin/awk -f
function sign(v){return (v>0)?1:(v<0?-1:0)}
function abs(v){return (v<0)?-v:v}
BEGIN{
    n=0
    while((getline line < "input.txt")>0){
        gsub(/[<>]/,"",line)
        split(line,a,/[,= ]+/)
        ++n
        posX[n]=a[2]; posY[n]=a[4]; posZ[n]=a[6]
        velX[n]=velY[n]=velZ[n]=0
    }
    close("input.txt")
    for(step=1;step<=1000;step++){
        for(i=1;i<=n;i++)for(j=i+1;j<=n;j++){
            dx=sign(posX[j]-posX[i]); dy=sign(posY[j]-posY[i]); dz=sign(posZ[j]-posZ[i])
            velX[i]+=dx; velY[i]+=dy; velZ[i]+=dz
            velX[j]-=dx; velY[j]-=dy; velZ[j]-=dz
        }
        for(i=1;i<=n;i++){
            posX[i]+=velX[i]; posY[i]+=velY[i]; posZ[i]+=velZ[i]
        }
    }
    total=0
    for(i=1;i<=n;i++){
        pot=abs(posX[i])+abs(posY[i])+abs(posZ[i])
        kin=abs(velX[i])+abs(velY[i])+abs(velZ[i])
        total+=pot*kin
    }
    print total
}
