
# solution.awk

BEGIN {
    FS=""
    x=0; y=0;
    while ((getline < "input.txt") > 0) {
        for (i=1; i<=length($0); i++) {
            dir = substr($0, i, 1)
            if (dir == "^") {
                y++
            } else if (dir == "v") {
                y--
            } else if (dir == ">") {
                x++
            } else if (dir == "<") {
                x--
            }
            visited[x","y]++
        }
    }
    print length(visited)
}
