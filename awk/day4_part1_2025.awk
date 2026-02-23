
BEGIN{
    rows=0
    while ((getline line < "input.txt") > 0) {
        if (line=="") continue
        rows++
        cols=length(line)
        for (i=1;i<=cols;i++) grid[rows,i]=substr(line,i,1)
    }
    close("input.txt")
    count=0
    for (y=1;y<=rows;y++) for (x=1;x<=cols;x++) if (grid[y,x]=="@") {
        neighbors=0
        for (dy=-1;dy<=1;dy++) {
            ny=y+dy
            if (ny<1||ny>rows) continue
            for (dx=-1;dx<=1;dx++) {
                nx=x+dx
                if (dx==0&&dy==0) continue
                if (nx<1||nx>cols) continue
                if (grid[ny,nx]=="@") neighbors++
            }
        }
        if (neighbors<4) count++
    }
    print count
}
