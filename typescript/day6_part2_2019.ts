import * as fs from 'fs';
import * as readline from 'readline';

class Node {
  name: string;
  children: Node[];
  parent: Node | null;

  constructor(name: string) {
    this.name = name;
    this.children = [];
    this.parent = null;
  }
}

function findOrCreateNode(name: string, nodes: Map<string, Node>): Node {
  if (nodes.has(name)) {
    return nodes.get(name)!;
  }
  const node = new Node(name);
  nodes.set(name, node);
  return node;
}

function buildOrbitMap(scanner: readline.Interface): Map<string, Node> {
  const nodes = new Map<string, Node>();
  scanner.on('line', (line) => {
    const parts = line.split(')');
    const center = findOrCreateNode(parts[0], nodes);
    const orbiter = findOrCreateNode(parts[1], nodes);
    center.children.push(orbiter);
    orbiter.parent = center;
  });
  return nodes;
}

function pathToRoot(node: Node): Node[] {
  const path: Node[] = [];
  while (node) {
    path.push(node);
    node = node.parent!;
  }
  return path;
}

function findCommonAncestor(node1: Node, node2: Node): [number, number] {
  const path1 = pathToRoot(node1);
  const path2 = pathToRoot(node2);

  let i = path1.length - 1;
  let j = path2.length - 1;

  while (i >= 0 && j >= 0 && path1[i] === path2[j]) {
    i--;
    j--;
  }
  return [i + 1, j + 1];
}

(async () => {
  const fileStream = fs.createReadStream('input.txt');
  const scanner = readline.createInterface({
    input: fileStream,
    crlfDelay: Infinity
  });

  const orbitMap = await new Promise<Map<string, Node>>((resolve) => {
    const nodes = buildOrbitMap(scanner);
    scanner.on('close', () => resolve(nodes));
  });

  const transfersYOU = findCommonAncestor(orbitMap.get('YOU')!.parent!, orbitMap.get('SAN')!.parent!);
  console.log(transfersYOU[0] + transfersYOU[1]);
})();