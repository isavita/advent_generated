#!/usr/bin/awk -f
BEGIN{
    input="input.txt"
    while((getline line < input) > 0){
        gsub(/,/,"",line)
        gsub(/ @ /," ",line)
        split(line,a," ")
        n++
        px[n]=a[1]; py[n]=a[2]; pz[n]=a[3]
        vx[n]=a[4]; vy[n]=a[5]; vz[n]=a[6]
    }
    close(input)
    solve_part1()
    solve_part2()
    exit
}
function solve_part1(){
    count=0
    minc=200000000000000.0
    maxc=400000000000000.0
    for(i=1;i<=n;i++){
        for(j=i+1;j<=n;j++){
            det=vx[i]*vy[j]-vy[i]*vx[j]
            if(det==0) continue
            t1=((px[j]-px[i])*vy[j]-(py[j]-py[i])*vx[j])/det
            t2=((px[j]-px[i])*vy[i]-(py[j]-py[i])*vx[i])/det
            if(t1>0 && t2>0){
                ix=px[i]+vx[i]*t1
                iy=py[i]+vy[i]*t1
                if(ix>=minc && ix<=maxc && iy>=minc && iy<=maxc){
                    count++
                }
            }
        }
    }
    printf("Part 1: %.0f\n", count)
}
function solve_part2(){
    if(n<3){
        printf("Part 2: Not enough data points.\n")
        return
    }
    for(i=1;i<=6;i++) for(j=1;j<=7;j++) A[i,j]=0
    h0=1; h1=2; h2=3
    dvx1=vx[h0]-vx[h1]; dvy1=vy[h0]-vy[h1]; dvz1=vz[h0]-vz[h1]
    dpx1=px[h0]-px[h1]; dpy1=py[h0]-py[h1]; dpz1=pz[h0]-pz[h1]
    dvx2=vx[h0]-vx[h2]; dvy2=vy[h0]-vy[h2]; dvz2=vz[h0]-vz[h2]
    dpx2=px[h0]-px[h2]; dpy2=py[h0]-py[h2]; dpz2=pz[h0]-pz[h2]
    A[1,2]=dvz1; A[1,3]=-dvy1; A[1,5]=dpz1; A[1,6]=-dpy1
    A[2,1]=-dvz1; A[2,3]=dvx1; A[2,4]=-dpz1; A[2,6]=dpx1
    A[3,1]=dvy1; A[3,2]=-dvx1; A[3,4]=dpy1; A[3,5]=-dpx1
    A[4,2]=dvz2; A[4,3]=-dvy2; A[4,5]=dpz2; A[4,6]=-dpy2
    A[5,1]=-dvz2; A[5,3]=dvx2; A[5,4]=-dpz2; A[5,6]=dpx2
    A[6,1]=dvy2; A[6,2]=-dvx2; A[6,4]=dpy2; A[6,5]=-dpx2
    A[1,7]=(py[h0]*vz[h0]-pz[h0]*vy[h0])-(py[h1]*vz[h1]-pz[h1]*vy[h1])
    A[2,7]=(pz[h0]*vx[h0]-px[h0]*vz[h0])-(pz[h1]*vx[h1]-px[h1]*vz[h1])
    A[3,7]=(px[h0]*vy[h0]-py[h0]*vx[h0])-(px[h1]*vy[h1]-py[h1]*vx[h1])
    A[4,7]=(py[h0]*vz[h0]-pz[h0]*vy[h0])-(py[h2]*vz[h2]-pz[h2]*vy[h2])
    A[5,7]=(pz[h0]*vx[h0]-px[h0]*vz[h0])-(pz[h2]*vx[h2]-px[h2]*vz[h2])
    A[6,7]=(px[h0]*vy[h0]-py[h0]*vx[h0])-(px[h2]*vy[h2]-py[h2]*vx[h2])
    for(i=1;i<=6;i++){
        max_row=i
        for(k=i+1;k<=6;k++) if((A[k,i]*A[k,i])>(A[max_row,i]*A[max_row,i])) max_row=k
        if(max_row!=i){
            for(j=1;j<=7;j++){
                tmp=A[i,j]; A[i,j]=A[max_row,j]; A[max_row,j]=tmp
            }
        }
        for(k=i+1;k<=6;k++){
            fac=A[k,i]/A[i,i]
            for(j=i;j<=7;j++) A[k,j]-=fac*A[i,j]
        }
    }
    for(i=6;i>=1;i--){
        for(j=i+1;j<=6;j++) A[i,7]-=A[i,j]*A[j,7]
        A[i,7]/=A[i,i]
    }
    prx=round(A[1,7]); pry=round(A[2,7]); prz=round(A[3,7])
    printf("Part 2: %.0f\n", prx+pry+prz)
}
function round(x){return (x>=0)?int(x+0.5):int(x-0.5)}