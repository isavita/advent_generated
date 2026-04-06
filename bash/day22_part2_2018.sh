
#!/bin/bash

awk '
BEGIN {
    while ((getline < "input.txt") > 0) {
        if ($0 ~ /depth:/) { split($0, a, ": "); dep = a[2] }
        if ($0 ~ /target:/) { 
            split($0, a, ": "); 
            split(a[2], pts, ","); 
            tx = pts[1]; ty = pts[2] 
        }
    }

    maxX = tx + 100
    maxY = ty + 100

    for (y = 0; y <= maxY; y++) {
        for (x = 0; x <= maxX; x++) {
            if ((x == 0 && y == 0) || (x == tx && y == ty)) g = 0
            else if (y == 0) g = x * 16807
            else if (x == 0) g = y * 48271
            else g = ero[y, x-1] * ero[y-1, x]
            ero[y, x] = (g + dep) % 20183
            reg[y, x] = ero[y, x] % 3
        }
    }

    risk = 0
    for (y = 0; y <= ty; y++) {
        for (x = 0; x <= tx; x++) risk += reg[y, x]
    }
    print "Total risk level: " risk

    dx[1]=0; dy[1]=1; dx[2]=0; dy[2]=-1; dx[3]=1; dy[3]=0; dx[4]=-1; dy[4]=0
    
    # tools: 0=neither, 1=torch, 2=climbing gear
    # dist[x, y, tool]
    dist[0, 0, 1] = 0
    q[0, ++qc[0]] = "0,0,1"
    
    for (t = 0; ; t++) {
        while (qc[t] > 0) {
            curr = q[t, qc[t]]; delete q[t, qc[t]--]
            split(curr, arr, ",")
            x = arr[1]; y = arr[2]; tl = arr[3]

            if (t > dist[x, y, tl] && (x, y, tl) in dist) continue
            if (x == tx && y == ty && tl == 1) {
                print "Fewest number of minutes to reach the target: " t
                exit
            }

            # Switch tools
            for (nt = 0; nt <= 2; nt++) {
                if (nt != tl && nt != reg[y, x]) {
                    if (!((x, y, nt) in dist) || t + 7 < dist[x, y, nt]) {
                        dist[x, y, nt] = t + 7
                        q[t + 7, ++qc[t + 7]] = x "," y "," nt
                    }
                }
            }

            # Move
            for (i = 1; i <= 4; i++) {
                nx = x + dx[i]; ny = y + dy[i]
                if (nx >= 0 && nx <= maxX && ny >= 0 && ny <= maxY) {
                    if (tl != reg[ny, nx]) {
                        if (!((nx, ny, tl) in dist) || t + 1 < dist[nx, ny, tl]) {
                            dist[nx, ny, tl] = t + 1
                            q[t + 1, ++qc[t + 1]] = nx "," ny "," tl
                        }
                    }
                }
            }
        }
        delete qc[t]
    }
}'
