
BEGIN{
    width=101; height=103; seconds=100
    midX=int(width/2); midY=int(height/2)
    n=0
    while ((getline line < "input.txt") > 0) {
        cnt=0
        while (match(line, /-?[0-9]+/)) {
            cnt++
            num[cnt]=substr(line, RSTART, RLENGTH)
            line=substr(line, RSTART+RLENGTH)
        }
        xs[++n]=num[1]; ys[n]=num[2]; vxs[n]=num[3]; vys[n]=num[4]
    }
}
END{
    for (s=1; s<=seconds; s++)
        for (i=1; i<=n; i++) {
            xs[i]+=vxs[i]; ys[i]+=vys[i]
            xs[i]=((xs[i]%width)+width)%width
            ys[i]=((ys[i]%height)+height)%height
        }
    q1=q2=q3=q4=0
    for (i=1; i<=n; i++) {
        x=xs[i]; y=ys[i]
        if (x<midX && y<midY) q1++
        else if (x>midX && y<midY) q2++
        else if (x<midX && y>midY) q3++
        else if (x>midX && y>midY) q4++
    }
    print q1*q2*q3*q4
}
