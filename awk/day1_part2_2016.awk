#!/usr/bin/awk -f
BEGIN {
    # direction vectors: 0=North,1=East,2=South,3=West
    dx[0]=0; dy[0]=1
    dx[1]=1; dy[1]=0
    dx[2]=0; dy[2]=-1
    dx[3]=-1; dy[3]=0

    x=0; y=0; dir=0
    visited["0,0"]=1

    while ((getline line < "input.txt") > 0) {
        n = split(line, a)
        for (i = 1; i <= n; i++) {
            instr = a[i]
                turn = substr(instr,1,1)
                steps = substr(instr,2)+0

                if (turn == "R")
                    dir = (dir + 1) % 4
                else
                    dir = (dir + 3) % 4   # (dir-1+4)%4

                stepX = dx[dir]; stepY = dy[dir]

                for (j = 0; j < steps; j++) {
                    x += stepX
                    y += stepY
                    key = x "," y
                    if (key in visited) {
                        print ( (x<0?-x:x) + (y<0?-y:y) )
                        exit
                    }
                    visited[key] = 1
                }
        }
    }
    print -1
    exit
}