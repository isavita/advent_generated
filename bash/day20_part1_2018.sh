
#!/bin/bash
awk '
{
    gsub(/^\^|\$$/, "")
    len = split($0, chars, "")
    x = y = p = max_dist = 0
    dist[0, 0] = 0
    for (i = 1; i <= len; i++) {
        c = chars[i]
        if (c == "(") {
            stack_x[p] = x
            stack_y[p] = y
            p++
        } else if (c == "|") {
            x = stack_x[p-1]
            y = stack_y[p-1]
        } else if (c == ")") {
            p--
            x = stack_x[p]
            y = stack_y[p]
        } else if (c ~ /[NSEW]/) {
            ox = x
            oy = y
            if (c == "N") y--
            else if (c == "S") y++
            else if (c == "E") x++
            else if (c == "W") x--
            
            if (!(x SUBSEP y in dist) || dist[ox, oy] + 1 < dist[x, y]) {
                dist[x, y] = dist[ox, oy] + 1
                if (dist[x, y] > max_dist) max_dist = dist[x, y]
            }
        }
    }
    print max_dist
}' input.txt
