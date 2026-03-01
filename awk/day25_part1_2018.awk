
#!/usr/bin/awk -f
BEGIN {
    fname = "input.txt"
    n = 0
    while ((getline line < fname) > 0) {
        n++
        split(line, a, ",")
        if (n == 1) dim = length(a)
        for (i = 1; i <= dim; i++) coord[n, i] = a[i] + 0
    }
    close(fname)

    for (i = 1; i <= n; i++) visited[i] = 0
    const = 0
    for (i = 1; i <= n; i++)
        if (!visited[i]) {
            dfs(i)
            const++
        }
    print const
}

function dist(a, b,    d, da, db, sum) {
    sum = 0
    for (d = 1; d <= dim; d++) {
        da = coord[a, d]
        db = coord[b, d]
        sum += (da > db ? da - db : db - da)
    }
    return sum
}

function dfs(start,    st, top, cur, i) {
    st[1] = start; top = 1
    visited[start] = 1
    while (top > 0) {
        cur = st[top]; top--
        for (i = 1; i <= n; i++)
            if (!visited[i] && dist(cur, i) <= 3) {
                st[++top] = i
                visited[i] = 1
            }
    }
}
