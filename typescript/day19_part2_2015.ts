import * as fs from 'fs';

function parseInput(input: string): [Map<string, string[]>, string[]] {
  const blocks = input.split('\n\n');
  const startingMaterial = splitMolecules(blocks[1]);
  const graph: Map<string, string[]> = new Map();

  for (const line of blocks[0].split('\n')) {
    const parts = line.split(' => ');
    if (!graph.has(parts[0])) {
      graph.set(parts[0], []);
    }
    graph.get(parts[0])!.push(parts[1]);
  }

  return [graph, startingMaterial];
}

function splitMolecules(input: string): string[] {
  const molecules: string[] = [];
  let currentMolecule = '';

  for (const char of input) {
    if (char >= 'A' && char <= 'Z') {
      if (currentMolecule) {
        molecules.push(currentMolecule);
      }
      currentMolecule = char;
    } else {
      currentMolecule += char;
    }
  }

  if (currentMolecule) {
    molecules.push(currentMolecule);
  }

  return molecules;
}

function solve(input: string): number {
  const [reverseGraph, startingMols] = parseInput(input);
  const productToReactant: Map<string, string> = new Map();

  for (const [react, products] of reverseGraph) {
    for (const product of products) {
      if (productToReactant.has(product)) {
        throw new Error('Duplicate product found');
      }
      productToReactant.set(product, react);
    }
  }

  const allProducts = Array.from(productToReactant.keys());
  let start = startingMols.join('');
  let mol = start;
  let steps = 0;

  while (mol !== 'e') {
    let changeMade = false;

    for (const product of allProducts) {
      const count = mol.split(product).length - 1;
      if (count <= 0) {
        continue;
      }
      changeMade = true;
      steps += count;
      mol = mol.replace(new RegExp(product, 'g'), productToReactant.get(product)!);

      break;
    }

    if (!changeMade) {
      allProducts.sort(() => Math.random() - 0.5);
      mol = start;
      steps = 0;
    }
  }

  return steps;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    throw err;
  }
  console.log(solve(data));
});