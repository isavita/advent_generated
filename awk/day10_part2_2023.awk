#!/usr/bin/awk -f
BEGIN {
    tile_to_pipe["|"] = "TB"
    tile_to_pipe["-"] = "RL"
    tile_to_pipe["J"] = "TL"
    tile_to_pipe["L"] = "TR"
    tile_to_pipe["7"] = "BL"
    tile_to_pipe["F"] = "BR"
    N = 0
    while ((getline line < "input.txt") > 0) {
        N++
        lines[N] = line
    }
    close("input.txt")
}
function opposite(d){
    if (d == "T") return "B"
    if (d == "B") return "T"
    if (d == "R") return "L"
    if (d == "L") return "R"
    return ""
}
function add_dir(coord, dir){
    split(coord, a, ",")
    x = a[1] + 0
    y = a[2] + 0
    if (dir == "T") y--
    else if (dir == "R") x++
    else if (dir == "B") y++
    else if (dir == "L") x--
    return x "," y
}
function get_pipe_from_tile(tile){
    if (tile in tile_to_pipe) return tile_to_pipe[tile]
    return ""
}
function get_pipe_from_neighbors(coord){
    p = ""
    dirs = "TRBL"
    for (i = 1; i <= length(dirs); i++){
        dir = substr(dirs, i, 1)
        nb = add_dir(coord, dir)
        t = grid[nb]
        if (t != ""){
            npipe = get_pipe_from_tile(t)
            if (index(npipe, opposite(dir)) > 0) p = p dir
        }
    }
    return p
}
function get_tile_from_pipe(pipe){
    for (t in tile_to_pipe){
        if (tile_to_pipe[t] == pipe) return t
    }
    return "."
}
function is_inside(coord, pgrid, empty_char){
    if (coord in pgrid) return 0
    split(coord, a, ",")
    cx = a[1] + 0
    cy = a[2] + 0
    start_pipe_char = empty_char
    num_left = 0
    for (xx = 0; xx < cx; xx++){
        cpos = xx "," cy
        v = pgrid[cpos]
        if (v != ""){
            if (v == "|") {
                num_left++
            } else if (v == "L") {
                start_pipe_char = "L"
            } else if (v == "F") {
                start_pipe_char = "F"
            } else if (v == "J") {
                if (start_pipe_char == "F") {
                    start_pipe_char = empty_char
                    num_left++
                } else if (start_pipe_char == "L") {
                    start_pipe_char = empty_char
                }
            } else if (v == "7") {
                if (start_pipe_char == "L") {
                    start_pipe_char = empty_char
                    num_left++
                } else if (start_pipe_char == "F") {
                    start_pipe_char = empty_char
                }
            }
        }
    }
    return (num_left % 2) == 1
}
END {
    max_y = N
    max_x = 0
    for (i = 1; i <= N; i++) if (length(lines[i]) > max_x) max_x = length(lines[i])

    # build grid (0-based coordinates)
    for (y = 0; y < max_y; y++) {
        line = lines[y+1]
        for (x = 0; x < length(line); x++) {
            ch = substr(line, x+1, 1)
            if (ch != ".") grid[x "," y] = ch
        }
    }

    # find start
    start = ""
    for (k in grid) if (grid[k] == "S") { start = k; break }

    # path_finding
    path_len = 0
    path[0] = start
    path_len = 1
    start_pipe = get_pipe_from_neighbors(start)
    if (length(start_pipe) > 0){
        dir = substr(start_pipe, 1, 1)
        prev = dir
        current = add_dir(start, dir)
        while (current != start){
            path[path_len] = current
            path_len++
            tile = grid[current]
            current_pipe = get_pipe_from_tile(tile)
            for (ii = 1; ii <= length(current_pipe); ii++){
                nd = substr(current_pipe, ii, 1)
                if (nd != opposite(prev)){
                    prev = nd
                    current = add_dir(current, nd)
                    break
                }
            }
        }
    }

    # path_grid
    for (i = 0; i < path_len; i++){
        coord = path[i]
        if (coord != "") path_grid[coord] = grid[coord]
    }
    start_pipe = get_pipe_from_neighbors(start)
    path_grid[start] = get_tile_from_pipe(start_pipe)

    # count inside cells
    count = 0
    for (y = 0; y < max_y; y++){
        for (x = 0; x < max_x; x++){
            coord = x "," y
            if (coord in path_grid) continue
            if (is_inside(coord, path_grid, ".")) count++
        }
    }
    print count
}