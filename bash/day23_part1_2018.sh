
awk -F'[^0-9-]+' '
{
    x[NR]=$2; y[NR]=$3; z[NR]=$4; r[NR]=$5
    if ($5 > max_r) {
        max_r=$5; mx=$2; my=$3; mz=$4
    }
}
END {
    for (i=1; i<=NR; i++) {
        dx = x[i]-mx; if(dx<0) dx=-dx
        dy = y[i]-my; if(dy<0) dy=-dy
        dz = z[i]-mz; if(dz<0) dz=-dz
        if (dx+dy+dz <= max_r) count++
    }
    print count
}' input.txt
