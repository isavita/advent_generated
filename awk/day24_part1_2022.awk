#!/usr/bin/awk -f
BEGIN {
    dx[0] = 0;  dy[0] = 1;
    dx[1] = 0;  dy[1] = -1;
    dx[2] = 1;  dy[2] = 0;
    dx[3] = -1; dy[3] = 0;
    dx[4] = 0;  dy[4] = 0;

    bliz[0] = "^";
    bliz[1] = ">";
    bliz[2] = "v";
    bliz[3] = "<";

    rows = 0; cols = 0;
    while ((getline line < "input.txt") > 0) {
        grid[rows] = line;
        if (cols == 0) cols = length(line);
        rows++;
    }
    close("input.txt");

    colsTotal = cols;
    rowsTotal = rows;

    interiorRows = rowsTotal - 2;
    interiorCols = colsTotal - 2;

    sX = 1; sY = 0;
    eX = colsTotal - 2; eY = rowsTotal - 1;

    head = 0; tail = 0;
    qx[tail] = sX; qy[tail] = sY; qst[tail] = 0; tail++;

    while (head < tail) {
        x = qx[head]; y = qy[head]; st = qst[head]; head++;

        if (x == eX && y == eY) {
            print st;
            exit;
        }

        for (dir = 0; dir < 5; dir++) {
            nx = x + dx[dir]; ny = y + dy[dir];
            nst = st + 1;

            if (nx < 0 || nx >= colsTotal || ny < 0 || ny >= rowsTotal) continue;
            if (substr(grid[ny], nx+1, 1) == "#") continue;

            key = ny "," nx "," (nst % (rowsTotal * colsTotal));
            if (key in seen) continue;

            valid = 1;
            if (ny > 0 && ny < rowsTotal - 1) {
                for (jj = 0; jj < 4; jj++) {
                    dirc = bliz[jj];
                    if (dirc == "^") {
                        px = nx;
                        py = (ny + nst) % interiorRows;
                        if (py == 0) py = interiorRows;
                    } else if (dirc == ">") {
                        px = (nx - nst) % interiorCols;
                        if (px < 0) px += interiorCols;
                        if (px == 0) px = interiorCols;
                        py = ny;
                    } else if (dirc == "v") {
                        px = nx;
                        py = (ny - nst) % interiorRows;
                        if (py < 0) py += interiorRows;
                        if (py == 0) py = interiorRows;
                    } else { # "<"
                        px = (nx + nst) % interiorCols;
                        if (px == 0) px = interiorCols;
                        py = ny;
                    }

                    if (substr(grid[py], px+1, 1) == dirc) {
                        valid = 0;
                        break;
                    }
                }
            }

            if (!valid) continue;

            seen[key] = 1;
            qx[tail] = nx; qy[tail] = ny; qst[tail] = nst; tail++;
        }
    }

    print -1;
}