
BEGIN {
    while ((getline line < "input.txt") > 0) {
        split(line, a, "[^0-9-]+")
        xMin=a[2]; xMax=a[3]; yMin=a[4]; yMax=a[5]
    }
    close("input.txt")

    for (vx=-500; vx<=xMax; vx++) {
        for (vy=yMin; vy<=-yMin; vy++) {
            x=0; y=0; cx=vx; cy=vy; hit=0
            while (1) {
                x+=cx; y+=cy
                if (x>=xMin && x<=xMax && y>=yMin && y<=yMax) { hit=1; break }
                if (x>xMax || y<yMin) break
                cx-=(cx>0)-(cx<0); cy--
            }
            tot+=hit
        }
    }
    print tot
}
