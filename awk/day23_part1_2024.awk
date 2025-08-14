#!/usr/bin/awk -f

BEGIN {
    FS = "-"
    node_count = 0
    while ((getline line < "input.txt") > 0) {
        if (line ~ /^$/) continue
        split(line, parts, "-")
        if (length(parts) != 2) continue
        name1 = parts[1]
        name2 = parts[2]

        if (!(name1 in id)) {
            id[name1] = ++node_count
            node[node_count] = name1
        }
        if (!(name2 in id)) {
            id[name2] = ++node_count
            node[node_count] = name2
        }
        i = id[name1]
        j = id[name2]
        adj[i","j] = 1
        adj[j","i] = 1
    }
    close("input.txt")
}

END {
    count = 0
    for (i = 1; i <= node_count; i++) {
        for (j = i + 1; j <= node_count; j++) {
            if (!((i","j) in adj)) continue
            for (k = j + 1; k <= node_count; k++) {
                if ((j","k) in adj && (k","i) in adj) {
                    if (substr(node[i],1,1) == "t" || substr(node[j],1,1) == "t" || substr(node[k],1,1) == "t")
                        count++
                }
            }
        }
    }
    print count
}