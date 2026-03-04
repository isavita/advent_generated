
BEGIN {
    FS = "[~,]"
    if (ARGC == 1) { ARGV[1] = "input.txt"; ARGC = 2 }
}
{
    n++
    x1[n]=$1; y1[n]=$2; z1[n]=$3; x2[n]=$4; y2[n]=$5; z2[n]=$6
    if(x1[n]>x2[n]){t=x1[n];x1[n]=x2[n];x2[n]=t}
    if(y1[n]>y2[n]){t=y1[n];y1[n]=y2[n];y2[n]=t}
    if(z1[n]>z2[n]){t=z1[n];z1[n]=z2[n];z2[n]=t}
}
END {
    for (i = 2; i <= n; i++) {
        tx1=x1[i]; ty1=y1[i]; tz1=z1[i]; tx2=x2[i]; ty2=y2[i]; tz2=z2[i]
        j = i - 1
        while (j >= 1 && z1[j] > tz1) {
            x1[j+1]=x1[j]; y1[j+1]=y1[j]; z1[j+1]=z1[j]
            x2[j+1]=x2[j]; y2[j+1]=y2[j]; z2[j+1]=z2[j]
            j--
        }
        x1[j+1]=tx1; y1[j+1]=ty1; z1[j+1]=tz1; x2[j+1]=tx2; y2[j+1]=ty2; z2[j+1]=tz2
    }
    for (i = 1; i <= n; i++) {
        mh = 0
        for (x = x1[i]; x <= x2[i]; x++)
            for (y = y1[i]; y <= y2[i]; y++)
                if (h[x,y] > mh) mh = h[x,y]
        split("", seen)
        for (x = x1[i]; x <= x2[i]; x++) {
            for (y = y1[i]; y <= y2[i]; y++) {
                if (h[x,y] == mh && top[x,y] > 0) {
                    s = top[x,y]
                    if (!seen[s]++) {
                        supps[s, ++ns[s]] = i
                        nb[i]++
                    }
                }
            }
        }
        dz = z2[i] - z1[i]
        z1[i] = mh + 1
        z2[i] = z1[i] + dz
        for (x = x1[i]; x <= x2[i]; x++)
            for (y = y1[i]; y <= y2[i]; y++) {
                h[x,y] = z2[i]
                top[x,y] = i
            }
    }
    for (i = 1; i <= n; i++) {
        safe = 1
        for (j = 1; j <= ns[i]; j++) {
            if (nb[supps[i,j]] == 1) {
                safe = 0
                break
            }
        }
        if (safe) ans++
    }
    print ans
}

