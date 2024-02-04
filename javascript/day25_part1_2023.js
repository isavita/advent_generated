const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

class Graph {
  constructor() {
    this.graph = {};
  }

  addEdge(start, end, weight) {
    if (!this.graph[start]) {
      this.graph[start] = {};
    }

    if (!this.graph[end]) {
      this.graph[end] = {};
    }

    this.graph[start][end] = weight;
    this.graph[end][start] = weight;
  }
}

function breadthFirstSearch(graph, start, goalFunc) {
  const frontier = [start];
  const reached = {[start]: true};
  const cameFrom = {[start]: start};

  while (frontier.length > 0) {
    const current = frontier.shift();

    if (goalFunc(current)) {
      return [true, cameFrom];
    }

    for (let next in graph[current]) {
      if (!reached[next]) {
        frontier.push(next);
        reached[next] = true;
        cameFrom[next] = current;
      }
    }
  }

  return [false, cameFrom];
}

function reconstructPath(start, end, cameFrom) {
  const path = [];
  let current = end;

  while (current !== start) {
    path.unshift(current);
    current = cameFrom[current];
  }

  path.unshift(start);
  return path;
}

function copyGraph(graph) {
  const newGraph = new Graph();

  for (let vertice in graph) {
    for (let edge in graph[vertice]) {
      newGraph.addEdge(vertice, edge, 1);
    }
  }

  return newGraph;
}

function solve(input) {
  const minCut = 3;

  const graph = new Graph();

  for (let line of input) {
    const parts = line.split(': ');
    const vertice = parts[0];
    const others = parts[1].split(' ');

    for (let other of others) {
      graph.addEdge(vertice, other, 1);
    }
  }

  let source;
  for (let vertice in graph.graph) {
    source = vertice;
    break;
  }

  let separateGraph;
  for (let end in graph.graph) {
    if (source === end) {
      continue;
    }

    const newGraph = copyGraph(graph.graph);
    for (let i = 0; i < minCut; i++) {
      const [isValid, cameFrom] = breadthFirstSearch(newGraph.graph, source, vertice => vertice === end);
      const path = reconstructPath(source, end, cameFrom);

      for (let j = 0; j < path.length - 1; j++) {
        delete newGraph.graph[path[j]][path[j + 1]];
      }
    }

    const [isValid] = breadthFirstSearch(newGraph.graph, source, vertice => vertice === end);
    if (!isValid) {
      separateGraph = newGraph;
      break;
    }
  }

  const [, cameFrom] = breadthFirstSearch(separateGraph.graph, source, vertice => false);
  const length1 = Object.keys(cameFrom).length;
  const length2 = Object.keys(separateGraph.graph).length - length1;

  return length1 * length2;
}

console.log(solve(input));