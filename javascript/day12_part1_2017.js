const fs = require('fs');

function DFS(node, adj, visited) {
    visited[node] = true;
    for (let neighbor of adj[node]) {
        if (!visited[neighbor]) {
            DFS(neighbor, adj, visited);
        }
    }
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const adj = {};
for (let line of input) {
    const parts = line.split(' <-> ');
    const from = parseInt(parts[0]);
    const toNodes = parts[1].split(', ');

    for (let toNode of toNodes) {
        const to = parseInt(toNode);
        adj[from] = adj[from] || [];
        adj[from].push(to);
        adj[to] = adj[to] || [];
        adj[to].push(from);
    }
}

const visited = {};
DFS(0, adj, visited);

let count = 0;
for (let v in visited) {
    if (visited[v]) {
        count++;
    }
}

console.log(count);