const fs = require('fs');

function DFS(node, adj, visited) {
    visited[node] = true;
    for (let neighbor of adj[node]) {
        if (!visited[neighbor]) {
            DFS(neighbor, adj, visited);
        }
    }
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error("File reading error", err);
        return;
    }

    const adj = {};
    const lines = data.split('\n');

    for (let line of lines) {
        const parts = line.split(' <-> ');
        const from = parseInt(parts[0]);
        const toNodes = parts[1].split(', ');

        for (let toNode of toNodes) {
            const to = parseInt(toNode);
            adj[from] = adj[from] || [];
            adj[to] = adj[to] || [];
            adj[from].push(to);
            adj[to].push(from);
        }
    }

    const visited = {};
    let groups = 0;

    for (let node in adj) {
        if (!visited[node]) {
            DFS(parseInt(node), adj, visited);
            groups++;
        }
    }

    console.log(groups);
});