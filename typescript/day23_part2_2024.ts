
const fs = require('fs');

const graph: { [key: string]: { [key: string]: boolean } } = {};
let bestClique: string[] = [];

function main() {
  const fileContent = fs.readFileSync('input.txt', 'utf-8');
  const lines = fileContent.split('\n');
  const nodesSet: { [key: string]: boolean } = {};

  for (const line of lines) {
    const parts = line.trim().split('-');
    if (parts.length !== 2) continue;
    const [a, b] = parts;
    if (!graph[a]) graph[a] = {};
    if (!graph[b]) graph[b] = {};
    graph[a][b] = true;
    graph[b][a] = true;
    nodesSet[a] = true;
    nodesSet[b] = true;
  }

  const allNodes = Object.keys(nodesSet);
  BronKerbosch([], allNodes, []);
  bestClique.sort();
  console.log(bestClique.join(','));
}

function BronKerbosch(R: string[], P: string[], X: string[]) {
  if (P.length === 0 && X.length === 0) {
    if (R.length > bestClique.length) {
      bestClique = [...R];
    }
    return;
  }

  const pivot = P.length > 0 ? P[0] : X[0];
  const pivotNeighbors = neighborsOf(pivot);
  const PWithoutPivotNeighbors = P.filter(v => !pivotNeighbors[v]);

  for (const v of PWithoutPivotNeighbors) {
    const neighbors = neighborsOf(v);
    BronKerbosch(
      union(R, v),
      intersect(P, neighbors),
      intersect(X, neighbors),
    );
    P = remove(P, v);
    X = union(X, v);
  }
}

function neighborsOf(node: string): { [key: string]: boolean } {
  return graph[node] || {};
}

function intersect(a: string[], b: { [key: string]: boolean }): string[] {
  return a.filter(x => b[x]);
}

function union(a: string[], x: string): string[] {
  return [...a, x];
}

function remove(slice: string[], s: string): string[] {
  return slice.filter(x => x !== s);
}

main();
