const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
const rules = input.split('\n').reduce((acc, line) => {
  const [container, contents] = line.split(' bags contain ');
  if (contents === 'no other bags.') return { ...acc, [container]: [] };
  
  const ruleRegex = /(\d+) (\w+ \w+) bags?[,.]/g;
  const matches = contents.matchAll(ruleRegex);
  const bagRules = [...matches].map(match => ({ color: match[2], count: Number(match[1]) }));
  
  return { ...acc, [container]: bagRules };
}, {});

console.log(countBags('shiny gold', rules) - 1);

function countBags(color, rules) {
  return 1 + rules[color].reduce((acc, rule) => acc + rule.count * countBags(rule.color, rules), 0);
}