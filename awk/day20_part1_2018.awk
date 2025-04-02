
#!/usr/bin/awk -f

BEGIN {
    FS=""
    getline line < "input.txt"
    len = length(line)
    regex = substr(line, 2, len - 2)
    len = length(regex)

    cx = 0; cy = 0
    sp = 0
    SUBSEP = ","

    for (i = 1; i <= len; ++i) {
        char = substr(regex, i, 1)

        if (char == "(") {
            stack[sp++] = cx SUBSEP cy
        } else if (char == "|") {
            split(stack[sp-1], pos, SUBSEP)
            cx = pos[1]; cy = pos[2]
        } else if (char == ")") {
             sp--
             split(stack[sp], pos, SUBSEP)
             cx = pos[1]; cy = pos[2]
        } else {
            ocx = cx; ocy = cy
            if (char == "N") cy--
            else if (char == "S") cy++
            else if (char == "E") cx++
            else if (char == "W") cx--

            # Store directed edge for BFS later
            adj[ocx SUBSEP ocy, cx SUBSEP cy] = 1
            # Need reverse edge as well for BFS exploration from a node
            adj[cx SUBSEP cy, ocx SUBSEP ocy] = 1
        }
    }

    # BFS to find furthest room
    qh = 0; qt = 0
    max_doors = 0
    start_pos = 0 SUBSEP 0
    queue[qt++] = start_pos
    dist[start_pos] = 0

    while (qh < qt) {
        curr = queue[qh++]
        split(curr, cpos, SUBSEP)
        ccx = cpos[1]; ccy = cpos[2]

        # Check potential neighbors based on cardinal directions
        # This avoids iterating the whole adj array, making BFS faster
        process_neighbor(ccx, ccy - 1, curr) # N
        process_neighbor(ccx, ccy + 1, curr) # S
        process_neighbor(ccx + 1, ccy, curr) # E
        process_neighbor(ccx - 1, ccy, curr) # W

    }

    print max_doors
    exit
}

function process_neighbor(nx, ny, curr,   npos, new_dist) {
     npos = nx SUBSEP ny
     # Check if an edge actually exists from curr to npos in the built map
     if ((curr SUBSEP npos) in adj && !(npos in dist)) {
          new_dist = dist[curr] + 1
          dist[npos] = new_dist
          if (new_dist > max_doors) {
              max_doors = new_dist
          }
          queue[qt++] = npos
     }
}
