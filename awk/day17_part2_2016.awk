#!/usr/bin/awk -f
BEGIN {
    getline passcode < "input.txt"
    passcode = passcode
    longest = 0
    dirs[1] = "U"; dirs[2] = "D"; dirs[3] = "L"; dirs[4] = "R"
    dx[1] = 0;  dy[1] = -1
    dx[2] = 0;  dy[2] = 1
    dx[3] = -1; dy[3] = 0
    dx[4] = 1;  dy[4] = 0
    dfs(0,0,"")
    print longest
}
function dfs(x, y, path,    hash_input, cmd, line, hash, i, dir, nx, ny, next_path) {
    if (x==3 && y==3) {
        if (length(path) > longest) longest = length(path)
        return
    }
    hash_input = passcode path
    cmd = "printf \"" hash_input "\" | md5sum"
    cmd | getline line
    close(cmd)
    split(line, arr, " ")
    hash = arr[1]
    for (i=1; i<=4; i++) {
        door = substr(hash, i, 1)
        if (door >= "b" && door <= "f") {
            dir = dirs[i]
            nx = x + dx[i]
            ny = y + dy[i]
            if (nx>=0 && nx<4 && ny>=0 && ny<4) {
                next_path = path dir
                dfs(nx, ny, next_path)
            }
        }
    }
}