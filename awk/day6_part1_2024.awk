
#!/usr/bin/awk -f

BEGIN {
    # read the whole grid from input.txt
    while ((getline line < "input.txt") > 0) {
        ++h
        grid[h] = line
        if (h == 1) w = length(line)
    }
    close("input.txt")

    # direction vectors: 0=up,1=right,2=down,3=left
    dx[0]=0;  dy[0]=-1
    dx[1]=1;  dy[1]=0
    dx[2]=0;  dy[2]=1
    dx[3]=-1; dy[3]=0

    # locate the robot and its initial direction
    for (i = 1; i <= h; ++i) {
        for (j = 1; j <= w; ++j) {
            c = substr(grid[i], j, 1)
            if (c == "^") { x=j; y=i; dir=0; break }
            if (c == ">") { x=j; y=i; dir=1; break }
            if (c == "v") { x=j; y=i; dir=2; break }
            if (c == "<") { x=j; y=i; dir=3; break }
        }
        if (dir != "") break
    }

    # count visited cells
    visited[y, x] = 1
    count = 1

    # walk until we leave the grid
    while (1) {
        nx = x + dx[dir]
        ny = y + dy[dir]

        if (nx < 1 || nx > w || ny < 1 || ny > h) break

        c = substr(grid[ny], nx, 1)

        if (c == "#") {
            dir = (dir + 1) % 4
            continue
        }

        x = nx; y = ny
        if (!((y, x) in visited)) {
            visited[y, x] = 1
            ++count
        }
    }

    print count
    exit
}
