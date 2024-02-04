const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

const reactPolymer = (polymer) => {
  let reacted = [];
  
  for (let unit of polymer) {
    if (reacted.length > 0 && unit !== reacted[reacted.length - 1] && unit.toUpperCase() === reacted[reacted.length - 1].toUpperCase()) {
      reacted.pop();
    } else {
      reacted.push(unit);
    }
  }
  
  return reacted.length;
};

console.log(reactPolymer(input));