const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

const adj = {};
const visited = {};

for (const line of input) {
  const [from, toNodes] = line.split(' <-> ');
  const fromNode = parseInt(from);
  const toNodeArray = toNodes.split(', ');

  for (const toNode of toNodeArray) {
    const to = parseInt(toNode);
    if (!adj[fromNode]) adj[fromNode] = [];
    if (!adj[to]) adj[to] = [];
    adj[fromNode].push(to);
    adj[to].push(fromNode);
  }
}

function DFS(node) {
  visited[node] = true;
  for (const neighbor of adj[node]) {
    if (!visited[neighbor]) DFS(neighbor);
  }
}

let groups = 0;
for (const node in adj) {
  if (!visited[node]) {
    DFS(node);
    groups++;
  }
}

console.log(groups);