import * as fs from 'fs';

interface Happiness {
  [key: string]: { [key: string]: number };
}

function parseInput(input: string): Happiness {
  const happiness: Happiness = {};
  const lines = input.trim().split('\n');

  lines.forEach(line => {
    const match = line.match(/^(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.$/);
    if (match) {
      const [, person1, action, units, person2] = match;
      const value = parseInt(units, 10) * (action === 'gain' ? 1 : -1);

      if (!happiness[person1]) {
        happiness[person1] = {};
      }
      happiness[person1][person2] = value;
    }
  });

  return happiness;
}

function calculateHappiness(arrangement: string[], happiness: Happiness): number {
  let totalHappiness = 0;

  for (let i = 0; i < arrangement.length; i++) {
    const person1 = arrangement[i];
    const person2 = arrangement[(i + 1) % arrangement.length];
    const person3 = arrangement[(i - 1 + arrangement.length) % arrangement.length];

    totalHappiness += happiness[person1][person2] + happiness[person1][person3];
  }

  return totalHappiness;
}

function findOptimalArrangement(people: string[], happiness: Happiness): number {
  let maxHappiness = -Infinity;

  function permute(arr: string[], m: number = 0): void {
    if (m === arr.length - 1) {
      maxHappiness = Math.max(maxHappiness, calculateHappiness(arr, happiness));
    } else {
      for (let i = m; i < arr.length; i++) {
        [arr[m], arr[i]] = [arr[i], arr[m]];
        permute(arr, m + 1);
        [arr[m], arr[i]] = [arr[i], arr[m]];
      }
    }
  }

  permute(people);
  return maxHappiness;
}

function main() {
  const input = fs.readFileSync('input.txt', 'utf-8');
  const happiness = parseInput(input);

  const people = Object.keys(happiness);
  const optimalHappiness = findOptimalArrangement(people, happiness);
  console.log(`Part 1: ${optimalHappiness}`);

  // Add yourself to the list
  const yourName = 'You';
  people.push(yourName);
  people.forEach(person => {
    happiness[yourName] = happiness[yourName] || {};
    happiness[yourName][person] = 0;
    happiness[person][yourName] = 0;
  });

  const optimalHappinessWithYou = findOptimalArrangement(people, happiness);
  console.log(`Part 2: ${optimalHappinessWithYou}`);
}

main();