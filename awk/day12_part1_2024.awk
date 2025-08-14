
#!/usr/bin/awk -f

BEGIN {
    # read the whole grid
    while ((getline line < "input.txt") > 0) {
        rows++
        grid[rows] = line
    }
    if (rows == 0) { print 0; exit }

    cols = length(grid[1])
    total = 0

    # BFS for each unvisited cell
    for (r = 1; r <= rows; r++) {
        for (c = 1; c <= cols; c++) {
            key = r SUBSEP c
            if (visited[key]) continue

            # start new region
            area = 0; per = 0
            head = 1; tail = 1
            q[1] = r SUBSEP c
            visited[key] = 1
            ch = substr(grid[r], c, 1)

            while (head <= tail) {
                split(q[head++], pos, SUBSEP)
                rr = pos[1]; cc = pos[2]
                area++

                # top
                if (rr > 1) {
                    if (substr(grid[rr-1], cc, 1) != ch) per++
                    else if (!visited[rr-1 SUBSEP cc]) {
                        q[++tail] = (rr-1) SUBSEP cc
                        visited[rr-1 SUBSEP cc] = 1
                    }
                } else per++

                # bottom
                if (rr < rows) {
                    if (substr(grid[rr+1], cc, 1) != ch) per++
                    else if (!visited[rr+1 SUBSEP cc]) {
                        q[++tail] = (rr+1) SUBSEP cc
                        visited[rr+1 SUBSEP cc] = 1
                    }
                } else per++

                # left
                if (cc > 1) {
                    if (substr(grid[rr], cc-1, 1) != ch) per++
                    else if (!visited[rr SUBSEP cc-1]) {
                        q[++tail] = rr SUBSEP (cc-1)
                        visited[rr SUBSEP cc-1] = 1
                    }
                } else per++

                # right
                if (cc < cols) {
                    if (substr(grid[rr], cc+1, 1) != ch) per++
                    else if (!visited[rr SUBSEP cc+1]) {
                        q[++tail] = rr SUBSEP (cc+1)
                        visited[rr SUBSEP cc+1] = 1
                    }
                } else per++
            }

            total += area * per
        }
    }

    print total
    exit
}
