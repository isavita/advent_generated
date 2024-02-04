const fs = require('fs');

const nodes = readNodes('input.txt');
const viablePairs = countViablePairs(nodes);
console.log(viablePairs);

function readNodes(filename) {
  const data = fs.readFileSync(filename, 'utf8');
  const lines = data.split('\n');
  const nodes = [];
  const nodeRegex = /node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%/;
  
  lines.forEach(line => {
    const matches = line.match(nodeRegex);
    if (matches) {
      const used = parseInt(matches[1]);
      const avail = parseInt(matches[2]);
      nodes.push({ used, avail });
    }
  });
  
  return nodes;
}

function countViablePairs(nodes) {
  let count = 0;
  
  nodes.forEach((a, i) => {
    nodes.forEach((b, j) => {
      if (i !== j && a.used > 0 && a.used <= b.avail) {
        count++;
      }
    });
  });
  
  return count;
}