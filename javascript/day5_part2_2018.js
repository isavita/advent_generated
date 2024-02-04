const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

const reactPolymer = (polymer) => {
  let reacted = [];
  
  for (let unit of polymer) {
    if (reacted.length === 0) {
      reacted.push(unit);
    } else {
      let prevUnit = reacted[reacted.length - 1];
      
      if (unit !== prevUnit && unit.toLowerCase() === prevUnit.toLowerCase()) {
        reacted.pop();
      } else {
        reacted.push(unit);
      }
    }
  }
  
  return reacted.join('');
};

const fullyReactPolymer = (polymer) => {
  let reacted = reactPolymer(polymer);
  
  while (polymer !== reacted) {
    polymer = reacted;
    reacted = reactPolymer(polymer);
  }
  
  return reacted;
};

const getShortestPolymerLength = (polymer) => {
  let shortest = polymer.length;
  
  for (let unit of 'abcdefghijklmnopqrstuvwxyz') {
    let modifiedPolymer = polymer.replace(new RegExp(unit, 'ig'), '');
    let reacted = fullyReactPolymer(modifiedPolymer);
    
    if (reacted.length < shortest) {
      shortest = reacted.length;
    }
  }
  
  return shortest;
};

const finalLength = getShortestPolymerLength(input);
console.log(finalLength);