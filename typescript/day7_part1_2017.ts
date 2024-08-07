import * as fs from 'fs';
import * as path from 'path';

interface Program {
  name: string;
  weight: number;
  children: string[];
}

function parseInput(input: string): Program[] {
  const lines = input.trim().split('\n');
  const programs: Program[] = [];

  for (const line of lines) {
    const nameWeightMatch = line.match(/^(\w+) \((\d+)\)/);
    if (!nameWeightMatch) throw new Error(`Invalid line: ${line}`);

    const name = nameWeightMatch[1];
    const weight = parseInt(nameWeightMatch[2], 10);
    const childrenMatch = line.match(/-> (.+)/);
    const children = childrenMatch ? childrenMatch[1].split(', ') : [];

    programs.push({ name, weight, children });
  }

  return programs;
}

function findBottomProgram(programs: Program[]): string {
  const allChildren = new Set<string>();
  const allNames = new Set<string>();

  for (const program of programs) {
    allNames.add(program.name);
    for (const child of program.children) {
      allChildren.add(child);
    }
  }

  for (const name of allNames) {
    if (!allChildren.has(name)) {
      return name;
    }
  }

  throw new Error('No bottom program found');
}

function main() {
  const filePath = path.join(__dirname, 'input.txt');
  const input = fs.readFileSync(filePath, 'utf-8');
  const programs = parseInput(input);
  const bottomProgram = findBottomProgram(programs);

  console.log(`The bottom program is: ${bottomProgram}`);
}

main();