#!/usr/bin/awk -f
BEGIN{
    FS="";
    rows=0;
    while ((getline line < "input.txt") > 0) {
        rows++;
        cols=length(line);
        for (i=1;i<=cols;i++) grid[rows,i]=substr(line,i,1)
    }
    part1=0;
    for (r=1;r<=rows;r++) {
        for (c=1;c<=cols;c++) {
            if (grid[r,c]!="@") continue;
            n=0;
            for (dr=-1;dr<=1;dr++) {
                nr=r+dr;
                if (nr<1||nr>rows) continue;
                for (dc=-1;dc<=1;dc++) {
                    if (dr==0 && dc==0) continue;
                    nc=c+dc;
                    if (nc<1||nc>cols) continue;
                    if (grid[nr,nc]=="@") n++
                }
            }
            if (n<4) part1++
        }
    }
    print part1;
    for (r=1;r<=rows;r++) for (c=1;c<=cols;c++) g[r,c]=grid[r,c];
    total=0;
    while (1) {
        delete toRemove;
        cnt=0;
        for (r=1;r<=rows;r++) {
            for (c=1;c<=cols;c++) {
                if (g[r,c]!="@") continue;
                n=0;
                for (dr=-1;dr<=1;dr++) {
                    nr=r+dr;
                    if (nr<1||nr>rows) continue;
                    for (dc=-1;dc<=1;dc++) {
                        if (dr==0 && dc==0) continue;
                        nc=c+dc;
                        if (nc<1||nc>cols) continue;
                        if (g[nr,nc]=="@") n++
                    }
                }
                if (n<4) toRemove[++cnt]=r SUBSEP c
            }
        }
        if (cnt==0) break;
        for (i=1;i<=cnt;i++) {
            split(toRemove[i],a,SUBSEP);
            g[a[1],a[2]]="."
        }
        total+=cnt
    }
    print total;
    exit
}