
function gcd(a, b) {
    if (b == 0)
        return a < 0 ? -a : a
    return gcd(b, a % b)
}

function solve() {
    h = 0
    w = 0
    delete antennas
    delete lines_per_freq
    delete antinodes

    while (getline line < "input.txt" > 0) {
        h++
        if (w == 0)
            w = length(line)
        for (x = 1; x <= w; x++) {
            c = substr(line, x, 1)
            if (c != ".") {
                antennas[c] = (antennas[c] == "" ? "" : antennas[c] SUBSEP) x SUBSEP h
            }
        }
    }

    split_len = 0;
    for (f in antennas) {
        split(antennas[f], coords_arr, SUBSEP)
        n = length(coords_arr) / 2
        lines_per_freq[f] = ""
        for (i = 1; i <= n; i++) {
            for (j = i + 1; j <= n; j++) {
                A_x = coords_arr[(i - 1) * 2 + 1]
                A_y = coords_arr[(i - 1) * 2 + 2]
                B_x = coords_arr[(j - 1) * 2 + 1]
                B_y = coords_arr[(j - 1) * 2 + 2]

                dy = B_y - A_y
                dx = B_x - A_x
                g = gcd(dy, dx)
                sy = dy / g
                sx = dx / g
                if (sx < 0 || (sx == 0 && sy < 0)) {
                    sx = -sx
                    sy = -sy
                }
                c = sy * A_x - sx * A_y

                line_str = sx SUBSEP sy SUBSEP c
                lines_per_freq[f] = (lines_per_freq[f] == "" ? "" : lines_per_freq[f] SUBSEP) line_str
            }
        }
    }
    
    antinode_count = 0;
    delete visited
    for (freq in lines_per_freq) {
        split(lines_per_freq[freq], lines_arr, SUBSEP)
        line_count = length(lines_arr) / 3
        for (i = 1; i <= line_count; i++) {
            sx = lines_arr[(i - 1) * 3 + 1]
            sy = lines_arr[(i - 1) * 3 + 2]
            c = lines_arr[(i - 1) * 3 + 3]
            
            if (sx == 0 && sy == 0)
                continue
            
            if (sy == 0) {
                if (c % sx == 0) {
                    y = -c / sx
                    if (y >= 1 && y <= h) {
                        for (x = 1; x <= w; x++) {
                            if (!visited[x SUBSEP y]) {
                                antinode_count++
                                visited[x SUBSEP y] = 1
                            }
                        }
                    }
                }
            } else if (sx == 0) {
                if (c % sy == 0) {
                    x = c / sy
                    if (x >= 1 && x <= w) {
                        for (y = 1; y <= h; y++) {
                            if (!visited[x SUBSEP y]) {
                                antinode_count++
                                visited[x SUBSEP y] = 1
                            }
                        }
                    }
                }
            } else {
                for (y = 1; y <= h; y++) {
                    val = c + sx * y
                    if (val % sy == 0) {
                        x = val / sy
                        if (x >= 1 && x <= w) {
                            if (!visited[x SUBSEP y]) {
                                antinode_count++
                                visited[x SUBSEP y] = 1
                            }
                        }
                    }
                }
            }
        }
    }

    print antinode_count
}

BEGIN {
    solve()
    exit
}
