const fs = require('fs');

function solve(input) {
  const parsed = parseInput(input);

  const graph = {};
  for (const [a, b] of parsed) {
    if (!graph[a]) graph[a] = new Set();
    if (!graph[b]) graph[b] = new Set();
    graph[a].add(b);
    graph[b].add(a);
  }

  return walk(graph, 'start', { 'start': 5 }, ['start'], false);
}

function walk(graph, current, visited, path, doubleUsed) {
  if (current === 'end') return 1;

  visited[current] = (visited[current] || 0) + 1;

  let pathsToEnd = 0;

  for (const visitable of graph[current]) {
    if (visitable === 'start') continue;

    if (/[a-z]/.test(visitable) && visited[visitable] > 0) {
      if (doubleUsed) continue;
      doubleUsed = true;
    }

    path.push(visitable);
    pathsToEnd += walk(graph, visitable, visited, path, doubleUsed);
    path.pop();

    visited[visitable]--;

    if (/[a-z]/.test(visitable) && visited[visitable] === 1) {
      doubleUsed = false;
    }
  }

  return pathsToEnd;
}

function parseInput(input) {
  return input.trim().split('\n').map(line => line.split('-'));
}

const input = fs.readFileSync('input.txt', 'utf8');
const answer = solve(input);
console.log(answer);