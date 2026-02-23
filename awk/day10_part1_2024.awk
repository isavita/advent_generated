#!/usr/bin/awk -f
BEGIN{
    nr=0
    while ((getline line < "input.txt") > 0) {
        nr++
        lines[nr]=line
        nc=length(line)
        for (i=1;i<=nc;i++) grid[nr,i]=substr(line,i,1)+0
    }
    close("input.txt")
    sum=0
    for (r=1;r<=nr;r++) for (c=1;c<=nc;c++) if (grid[r,c]==0) {
        delete visited; delete reached
        head=1; tail=1
        qx[head]=r; qy[head]=c; qh[head]=0
        visited[r SUBSEP c SUBSEP 0]=1
        while (head<=tail) {
            cr=qx[head]; cc=qy[head]; ch=qh[head]; head++
            if (ch==9) {
                reached[cr SUBSEP cc]=1
            } else {
                nh=ch+1
                if (cr>1 && grid[cr-1,cc]==nh && !((cr-1 SUBSEP cc SUBSEP nh) in visited)) {
                    tail++; qx[tail]=cr-1; qy[tail]=cc; qh[tail]=nh; visited[cr-1 SUBSEP cc SUBSEP nh]=1
                }
                if (cr<nr && grid[cr+1,cc]==nh && !((cr+1 SUBSEP cc SUBSEP nh) in visited)) {
                    tail++; qx[tail]=cr+1; qy[tail]=cc; qh[tail]=nh; visited[cr+1 SUBSEP cc SUBSEP nh]=1
                }
                if (cc>1 && grid[cr,cc-1]==nh && !((cr SUBSEP cc-1 SUBSEP nh) in visited)) {
                    tail++; qx[tail]=cr; qy[tail]=cc-1; qh[tail]=nh; visited[cr SUBSEP cc-1 SUBSEP nh]=1
                }
                if (cc<nc && grid[cr,cc+1]==nh && !((cr SUBSEP cc+1 SUBSEP nh) in visited)) {
                    tail++; qx[tail]=cr; qy[tail]=cc+1; qh[tail]=nh; visited[cr SUBSEP cc+1 SUBSEP nh]=1
                }
            }
        }
        for (k in reached) sum++
    }
    print sum
}