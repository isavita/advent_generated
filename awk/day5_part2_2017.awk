bash
#!/usr/bin/awk -f

{
    maze[NR] = $1
}
END {
    steps = 0
    pos = 1

    while (pos >= 1 && pos <= NR) {
        offset = maze[pos]
        maze[pos] += (offset >= 3) ? -1 : 1
        pos += offset
        steps++
    }

    print steps
}
