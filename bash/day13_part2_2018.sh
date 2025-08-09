#!/usr/bin/env bash
awk '
{
    lines[NR-1]=$0
    for(i=1;i<=length($0);i++){
        c=substr($0,i,1)
        if(c ~ /[<>^v]/){
            carts[count++] = i-1 " " NR-1 " " c
            if(c==">"||c=="<") $0=substr($0,1,i-1) (c==">"||c=="<"? "-":"|") substr($0,i+1)
        }
    }
}
END{
    for(i=0;i<NR;i++) track[i]=lines[i]
    for(i=0;i<count;i++){
        split(carts[i],a," ")
        xs[i]=a[1]; ys[i]=a[2]; ds[i]=a[3]; ts[i]=0
    }
    while(length(xs)>1){
        n=length(xs)
        for(i=0;i<n-1;i++)for(j=i+1;j<n;j++)if(ys[i]>ys[j]||(ys[i]==ys[j]&&xs[i]>xs[j])){
            t=xs[i]; xs[i]=xs[j]; xs[j]=t
            t=ys[i]; ys[i]=ys[j]; ys[j]=t
            t=ds[i]; ds[i]=ds[j]; ds[j]=t
            t=ts[i]; ts[i]=ts[j]; ts[j]=t
        }
        delete toRemove
        for(i=0;i<n;i++){
            if(toRemove[i])continue
            d=ds[i]
            if(d==">")xs[i]++
            else if(d=="<")xs[i]--
            else if(d=="^")ys[i]--
            else if(d=="v")ys[i]++
            piece=substr(track[ys[i]],xs[i]+1,1)
            if(piece=="+"){
                r=ts[i]%3
                if(r==0){
                    if(d==">")nd="^"
                    else if(d=="<")nd="v"
                    else if(d=="^")nd="<"
                    else nd=">"
                }else if(r==2){
                    if(d==">")nd="v"
                    else if(d=="<")nd="^"
                    else if(d=="^")nd=">"
                    else nd="<"
                }else{nd=d}
                ds[i]=nd
                ts[i]++
            }else if(piece=="/"){
                if(d==">")nd="^"
                else if(d=="<")nd="v"
                else if(d=="^")nd=">"
                else nd="<"
                ds[i]=nd
            }else if(piece=="\\"){
                if(d==">")nd="v"
                else if(d=="<")nd="^"
                else if(d=="^")nd="<"
                else nd=">"
                ds[i]=nd
            }
            for(j=0;j<n;j++)if(j!=i&&xs[i]==xs[j]&&ys[i]==ys[j]){
                toRemove[i]=1;toRemove[j]=1;break
            }
        }
        delete newX;delete newY;delete newD;delete newT
        n2=0
        for(i=0;i<n;i++)if(!toRemove[i]){
            newX[n2]=xs[i];newY[n2]=ys[i];newD[n2]=ds[i];newT[n2]=ts[i];n2++
        }
        delete xs;delete ys;delete ds;delete ts
        for(i=0;i<n2;i++){
            xs[i]=newX[i];ys[i]=newY[i];ds[i]=newD[i];ts[i]=newT[i]
        }
    }
    print xs[0] "," ys[0]
}
' input.txt