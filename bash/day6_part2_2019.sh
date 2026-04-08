
awk -F')' '
{
    parent[$2] = $1
}
END {
    curr = parent["YOU"]
    while (curr) {
        dist[curr] = i++
        curr = parent[curr]
    }
    curr = parent["SAN"]
    while (!(curr in dist)) {
        curr = parent[curr]
        j++
    }
    print j + dist[curr]
}' input.txt
