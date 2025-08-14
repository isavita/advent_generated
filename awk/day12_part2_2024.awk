
#!/usr/bin/awk -f
# ------------------------------------------------------------
#  Plant‑region price calculator (awk version)
#  Reads the map from the file "input.txt" and prints:
#      Part 1 Total Price: <value>
#      Part 2 Total Price: <value>
# ------------------------------------------------------------

BEGIN {
    # ---------- configuration ----------
    MAX_DIM   = 200                     # maximum grid size
    PADDING   = 1
    GRID_DIM  = MAX_DIM + 2 * PADDING    # padded dimension

    # ---------- data structures ----------
    # grid[r,c]   : character at (r,c) (1‑based, padded)
    # visited[r,c] : 1 if cell already processed
    # is_in_region[r,c] : temporary flag for current region

    # read the whole file into the padded grid
    while ((getline line < "input.txt") > 0) {
        if (height >= MAX_DIM) { exit 1 }
        sub(/\r?\n$/, "", line)               # strip newline
        if (length(line) == 0) continue
        if (width == 0) width = length(line)
        ++height
        for (c = 1; c <= width; ++c) {
            grid[height + PADDING, c + PADDING] = substr(line, c, 1)
        }
    }
    close("input.txt")
    if (height == 0 || width == 0) {
        print "Error: empty input" > "/dev/stderr"
        exit 1
    }

    # fill padding with a non‑plant character ('.')
    for (r = 0; r <= height + 2 * PADDING; ++r)
        for (c = 0; c <= width + 2 * PADDING; ++c)
            if (!((r, c) in grid)) grid[r,c] = "."

    # ------------------------------------------------------------
    #  main processing: find all connected regions
    # ------------------------------------------------------------
    total1 = 0
    total2 = 0

    for (r0 = 1 + PADDING; r0 <= height + PADDING; ++r0) {
        for (c0 = 1 + PADDING; c0 <= width + PADDING; ++c0) {
            if (visited[r0,c0]) continue

            plant = grid[r0,c0]

            # ---- BFS ----
            qh = 0; qt = 0                     # queue head / tail
            queue_r[qt] = r0;  queue_c[qt] = c0; ++qt
            visited[r0,c0] = 1

            area = 0; perimeter = 0
            region_sz = 0

            while (qh < qt) {
                r = queue_r[qh]; c = queue_c[qh]; ++qh
                ++area
                region_r[region_sz] = r
                region_c[region_sz] = c
                ++region_sz

                # four neighbours
                nr = r - 1; nc = c
                if (grid[nr,nc] != plant) ++perimeter
                else if (!visited[nr,nc]) {
                    visited[nr,nc] = 1
                    queue_r[qt] = nr; queue_c[qt] = nc; ++qt
                }

                nr = r + 1; nc = c
                if (grid[nr,nc] != plant) ++perimeter
                else if (!visited[nr,nc]) {
                    visited[nr,nc] = 1
                    queue_r[qt] = nr; queue_c[qt] = nc; ++qt
                }

                nr = r; nc = c - 1
                if (grid[nr,nc] != plant) ++perimeter
                else if (!visited[nr,nc]) {
                    visited[nr,nc] = 1
                    queue_r[qt] = nr; queue_c[qt] = nc; ++qt
                }

                nr = r; nc = c + 1
                if (grid[nr,nc] != plant) ++perimeter
                else if (!visited[nr,nc]) {
                    visited[nr,nc] = 1
                    queue_r[qt] = nr; queue_c[qt] = nc; ++qt
                }
            }

            # ---- part 1 : area * perimeter ----
            total1 += area * perimeter

            # ---- part 2 : area * sides ----
            # reset temporary region map
            for (i = 0; i < region_sz; ++i) {
                r = region_r[i]; c = region_c[i]
                is_in_region[r,c] = 1
            }

            top = bottom = left = right = 0
            top_adj = bottom_adj = left_adj = right_adj = 0

            for (i = 0; i < region_sz; ++i) {
                r = region_r[i]; c = region_c[i]

                if (grid[r-1,c] != plant) ++top
                if (grid[r+1,c] != plant) ++bottom
                if (grid[r,c-1] != plant) ++left
                if (grid[r,c+1] != plant) ++right

                # horizontal adjacency (right neighbour)
                if (is_in_region[r,c+1]) {
                    if (grid[r-1,c] != plant && grid[r-1,c+1] != plant) ++top_adj
                    if (grid[r+1,c] != plant && grid[r+1,c+1] != plant) ++bottom_adj
                }
                # vertical adjacency (down neighbour)
                if (is_in_region[r+1,c]) {
                    if (grid[r,c-1] != plant && grid[r+1,c-1] != plant) ++left_adj
                    if (grid[r,c+1] != plant && grid[r+1,c+1] != plant) ++right_adj
                }
            }

            sides = (top - top_adj) + (bottom - bottom_adj) + (left - left_adj) + (right - right_adj)
            total2 += area * sides

            # clear temporary map for next region
            for (i = 0; i < region_sz; ++i) {
                r = region_r[i]; c = region_c[i]
                delete is_in_region[r,c]
            }
        }
    }

    print "Part 1 Total Price: " total1
    print "Part 2 Total Price: " total2
    exit 0
}
