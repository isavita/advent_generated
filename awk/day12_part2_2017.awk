
{
    split($0, arr, " <-> ");
    split(arr[2], neighbors, ", ");
    for (i in neighbors) {
        graph[arr[1], neighbors[i]] = 1;
        graph[neighbors[i], arr[1]] = 1;
    }
}

function dfs(node, visited, graph) {
    visited[node] = 1;
    for (neighbor in graph) {
        split(neighbor, nodes, SUBSEP);
        if (nodes[1] == node && !(nodes[2] in visited)) {
            dfs(nodes[2], visited, graph);
        }
    }
}

END {
    count = 0;
    for (node in graph) {
        split(node, nodes, SUBSEP);
        if (!(nodes[1] in visited)) {
            dfs(nodes[1], visited, graph);
            count++;
        }
    }
    print count;
}
