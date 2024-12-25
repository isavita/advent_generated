
const fs = require('fs');

const graph: { [key: string]: { [key: string]: boolean } } = {};

const addConnection = (from: string, to: string) => {
  if (!graph[from]) {
    graph[from] = {};
  }
  graph[from][to] = true;
};

const data = fs.readFileSync('input.txt', 'utf-8');
const lines = data.split('\n');

for (const line of lines) {
  const computers = line.trim().split('-');
  if (computers.length === 2) {
    addConnection(computers[0], computers[1]);
    addConnection(computers[1], computers[0]);
  }
}

let count = 0;
const computers = Object.keys(graph);
const n = computers.length;

for (let i = 0; i < n; i++) {
  for (let j = i + 1; j < n; j++) {
    if (!graph[computers[i]][computers[j]]) continue;
    for (let k = j + 1; k < n; k++) {
      if (
        graph[computers[i]][computers[k]] &&
        graph[computers[j]][computers[k]] &&
        (computers[i].startsWith('t') ||
          computers[j].startsWith('t') ||
          computers[k].startsWith('t'))
      ) {
        count++;
      }
    }
  }
}

console.log(count);
