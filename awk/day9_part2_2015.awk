
#!/usr/bin/awk -f
#  awk script to compute the longest route distance
#  Reads data from "input.txt"

BEGIN {
    # Read the input file
    while ((getline line < "input.txt") > 0) {
        # Expected format:  London to Dublin = 464
        split(line, a, " ")
        from = a[1]
        to   = a[3]
        d    = a[5]

        dist[from,to] = d
        dist[to,from] = d

        if (!(from in locs)) { locs[from] = 1; names[++n] = from }
        if (!(to   in locs)) { locs[to]   = 1; names[++n] = to   }
    }

    # initialise visited array
    for (i = 1; i <= n; i++) visited[names[i]] = 0

    max = 0
    permute(0, 0)
    print max
}

# Recursive permutation generator
function permute(depth, sum,    i, name, prev, newSum) {
    if (depth == n) {
        if (sum > max) max = sum
        return
    }
    for (i = 1; i <= n; i++) {
        name = names[i]
        if (!visited[name]) {
            visited[name] = 1
            route[depth] = name
            if (depth == 0) {
                newSum = sum
            } else {
                prev = route[depth-1]
                newSum = sum + dist[prev,name]
            }
            permute(depth+1, newSum)
            visited[name] = 0
        }
    }
}
