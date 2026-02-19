
#!/usr/bin/awk -f

BEGIN {
    # read passcode
    while ((getline line < "input.txt") > 0) {
        passcode = line
    }
    close("input.txt")

    # BFS queue
    qhead = 1
    qtail = 1
    qx[1] = 0; qy[1] = 0; qp[1] = ""

    while (qhead <= qtail) {
        x = qx[qhead]
        y = qy[qhead]
        path = qp[qhead]
        qhead++

        if (x == 3 && y == 3) {
            print path
            exit
        }

        # MD5 hash of passcode+path
        cmd = "printf \"%s\" \"" passcode path "\" | md5sum"
        cmd | getline hashline
        close(cmd)
        split(hashline, parts, " ")
        hash = parts[1]

        dirs = ""
        if (substr(hash,1,1) ~ /[b-f]/) dirs = dirs "U"
        if (substr(hash,2,1) ~ /[b-f]/) dirs = dirs "D"
        if (substr(hash,3,1) ~ /[b-f]/) dirs = dirs "L"
        if (substr(hash,4,1) ~ /[b-f]/) dirs = dirs "R"

        for (i = 1; i <= length(dirs); i++) {
            dir = substr(dirs, i, 1)
            nx = x; ny = y; npath = path dir

            if (dir == "U") ny--
            else if (dir == "D") ny++
            else if (dir == "L") nx--
            else if (dir == "R") nx++

            if (nx >= 0 && nx < 4 && ny >= 0 && ny < 4) {
                qtail++
                qx[qtail] = nx
                qy[qtail] = ny
                qp[qtail] = npath
            }
        }
    }

    print "No path found"
}
