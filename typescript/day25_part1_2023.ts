import * as fs from 'fs';

type Vertice = string;

interface Edge {
  start: Vertice;
  end: Vertice;
  weight: number;
}

type Graph = {
  [key: Vertice]: {
    [key: string]: {};
  };
};

function parseInput(input: string[]): Graph {
  const weight = 1;
  const graph: Graph = {};

  input.forEach(line => {
    const [vertice, others] = line.split(': ');
    const otherVertices = others.split(' ');

    if (!graph[vertice]) {
      graph[vertice] = {};
    }

    otherVertices.forEach(other => {
      const otherVertice = other;
      if (!graph[otherVertice]) {
        graph[otherVertice] = {};
      }

      const edge1 = JSON.stringify({ start: vertice, end: otherVertice, weight });
      const edge2 = JSON.stringify({ start: otherVertice, end: vertice, weight });
      graph[vertice][edge1] = {};
      graph[otherVertice][edge2] = {};
    });
  });

  return graph;
}

function breadthFirstSearch(graph: Graph, start: Vertice, goalFunc: (vertice: Vertice) => boolean): [boolean, { [key: Vertice]: Vertice }] {
  const frontier = [start];
  const reached = { [start]: {} };
  const cameFrom = { [start]: start };

  while (frontier.length > 0) {
    const current = frontier.shift()!;

    if (goalFunc(current)) {
      return [true, cameFrom];
    }

    for (const edge of Object.keys(graph[current])) {
      const { end } = JSON.parse(edge) as Edge;
      if (!reached[end]) {
        frontier.push(end);
        reached[end] = {};
        cameFrom[end] = current;
      }
    }
  }

  return [false, cameFrom];
}

function reconstructPath(start: Vertice, end: Vertice, cameFrom: { [key: Vertice]: Vertice }): Vertice[] {
  const path: Vertice[] = [];
  let current = end;
  while (current !== start) {
    path.unshift(current);
    current = cameFrom[current];
  }
  path.unshift(start);
  return path;
}

function copyGraph(graph: Graph): Graph {
  const newGraph: Graph = {};
  for (const vertice in graph) {
    newGraph[vertice] = {};
    for (const edge in graph[vertice]) {
      newGraph[vertice][edge] = {};
    }
  }
  return newGraph;
}

function solve(input: string[]): number {
  const minCut = 3;
  const graph = parseInput(input);

  let source: Vertice = Object.keys(graph)[0];

  let separateGraph: Graph | undefined;
  for (const end in graph) {
    if (source === end) {
      continue;
    }

    let newGraph = copyGraph(graph);
    for (let i = 0; i < minCut; i++) {
      const [_, cameFrom] = breadthFirstSearch(newGraph, source, vertice => vertice === end);
      const path = reconstructPath(source, end, cameFrom);
      for (let j = 0; j < path.length - 1; j++) {
        const edge = JSON.stringify({ start: path[j], end: path[j + 1], weight: 1 });
        delete newGraph[path[j]][edge];
      }
    }

    const [isValid] = breadthFirstSearch(newGraph, source, vertice => vertice === end);
    if (!isValid) {
      separateGraph = newGraph;
      break;
    }
  }

  if (!separateGraph) {
    throw new Error('No separate graph found');
  }

  const [_, cameFrom] = breadthFirstSearch(separateGraph, source, () => false);
  const length1 = Object.keys(cameFrom).length;
  const length2 = Object.keys(separateGraph).length - length1;

  return length1 * length2;
}

function readFile(fileName: string): string[] {
  const data = fs.readFileSync(fileName, 'utf8');
  return data.trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));