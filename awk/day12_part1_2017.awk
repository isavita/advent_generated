BEGIN {
    # Read input and build the adjacency list
    while (getline < "input.txt") {
        split($0, parts, " <-> ");
        id = parts[1];
        split(parts[2], neighbors, ", ");
        for (n in neighbors) {
            graph[id "," neighbors[n]] = 1;  # Store bidirectional relationship
            graph[neighbors[n] "," id] = 1;  # Store bidirectional relationship
        }
    }
    
    # Initialize visited array and DFS
    for (k in graph) {
        split(k, nodes, ",");
        visited[nodes[1]] = 0;
        visited[nodes[2]] = 0;
    }
    
    count = 0;
    dfs("0");  # Start DFS from node "0"
    print count;
}

function dfs(node) {
    if (visited[node]) return;
    visited[node] = 1;
    count++;
    
    for (k in graph) {
        split(k, nodes, ",");
        if (nodes[1] == node) {
            dfs(nodes[2]);
        } else if (nodes[2] == node) {
            dfs(nodes[1]);
        }
    }
}