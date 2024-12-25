
const fs = require('fs');

const solve = () => {
  const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
  const graph = {};
  const nodes = new Set();

  for (const line of input) {
    const [a, b] = line.split('-');
    if (!a || !b) continue;
    graph[a] = graph[a] || {};
    graph[b] = graph[b] || {};
    graph[a][b] = true;
    graph[b][a] = true;
    nodes.add(a);
    nodes.add(b);
  }

  const allNodes = Array.from(nodes);
  let bestClique = [];

  const bronKerbosch = (r, p, x) => {
    if (p.length === 0 && x.length === 0) {
      if (r.length > bestClique.length) {
        bestClique = [...r];
      }
      return;
    }

    const tempP = [...p];
    for (const v of tempP) {
      const neighbors = graph[v] ? Object.keys(graph[v]) : [];
      bronKerbosch(
        [...r, v],
        p.filter(n => neighbors.includes(n)),
        x.filter(n => neighbors.includes(n))
      );
      p = p.filter(n => n !== v);
      x.push(v);
    }
  };

  bronKerbosch([], allNodes, []);
  bestClique.sort();
  console.log(bestClique.join(','));
};

solve();
