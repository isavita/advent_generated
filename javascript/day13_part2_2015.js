const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const people = new Set();
const happiness = new Map();

for (const line of input) {
  const [person1, action, amount, person2] = line.match(/(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\./).slice(1);
  people.add(person1);
  people.add(person2);
  happiness.set(`${person1}-${person2}`, action === 'gain' ? +amount : -amount);
}

const findOptimalArrangement = (people) => {
  const arrangements = [];
  const backtrack = (arrangement, remaining) => {
    if (remaining.size === 0) {
      arrangements.push([...arrangement]);
      return;
    }
    for (const person of remaining) {
      arrangement.push(person);
      const next = new Set(remaining);
      next.delete(person);
      backtrack(arrangement, next);
      arrangement.pop();
    }
  };
  backtrack([], people);
  return arrangements.reduce((max, arrangement) => {
    let total = 0;
    for (let i = 0; i < arrangement.length; i++) {
      const prev = arrangement[i - 1] || arrangement[arrangement.length - 1];
      const next = arrangement[(i + 1) % arrangement.length];
      total += happiness.get(`${arrangement[i]}-${prev}`) || 0;
      total += happiness.get(`${arrangement[i]}-${next}`) || 0;
    }
    return Math.max(max, total);
  }, 0);
};

console.log(findOptimalArrangement(people));

people.add('me');
for (const person of people) {
  happiness.set(`me-${person}`, 0);
  happiness.set(`${person}-me`, 0);
}

console.log(findOptimalArrangement(people));