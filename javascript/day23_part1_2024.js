
const fs = require('fs');

const solve = () => {
  const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
  const graph = {};

  for (const line of input) {
    const [from, to] = line.split('-');
    if (!from || !to) continue;
    graph[from] = graph[from] || {};
    graph[to] = graph[to] || {};
    graph[from][to] = true;
    graph[to][from] = true;
  }

  const computers = Object.keys(graph);
  const seen = new Set();
  let count = 0;

  for (let i = 0; i < computers.length; i++) {
    for (let j = i + 1; j < computers.length; j++) {
      for (let k = j + 1; k < computers.length; k++) {
        const c1 = computers[i];
        const c2 = computers[j];
        const c3 = computers[k];

        if (graph[c1]?.[c2] && graph[c2]?.[c3] && graph[c1]?.[c3]) {
          if (c1.startsWith('t') || c2.startsWith('t') || c3.startsWith('t')) {
            const triplet = [c1, c2, c3].sort().join(',');
            if (!seen.has(triplet)) {
              seen.add(triplet);
              count++;
            }
          }
        }
      }
    }
  }
  console.log("Number of triplets containing at least one computer with name starting with 't':", count);
};

solve();
