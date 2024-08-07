import * as fs from 'fs';
import * as path from 'path';

const input = fs.readFileSync(path.join(__dirname, 'input.txt'), 'utf8');
const lines = input.split('\n');

const rules: { [key: number]: string } = {};
let i = 0;
while (lines[i] !== '') {
  const [key, value] = lines[i].split(': ');
  rules[parseInt(key)] = value.replace(/"/g, '');
  i++;
}

function constructPattern(index: number): string {
  if (rules[index].includes('|')) {
    const subrules = rules[index].split(' | ');
    const parts = subrules.map(constructSubPattern);
    return `(${parts.join('|')})`;
  }
  return constructSubPattern(rules[index]);
}

function constructSubPattern(subrule: string): string {
  if (subrule === 'a' || subrule === 'b') {
    return subrule;
  }
  const subIdxs = subrule.split(' ');
  let pattern = '';
  for (const idx of subIdxs) {
    pattern += constructPattern(parseInt(idx));
  }
  return pattern;
}

const pattern = constructPattern(0);
const regex = new RegExp(`^${pattern}$`);
let count = 0;
for (let j = i + 1; j < lines.length; j++) {
  if (regex.test(lines[j])) {
    count++;
  }
}

console.log(`The number of messages that completely match rule 0 is: ${count}`);