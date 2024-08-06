const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8');

const happinessMap = parseHappinessValues(input);
const guests = getGuestList(happinessMap);
const maxHappiness = calculateOptimalArrangement(guests, happinessMap);

console.log(maxHappiness);

function parseHappinessValues(input) {
  const happinessMap = {};
  input.split('\n').forEach(line => {
    const parts = line.split(' ');
    if (parts.length < 11) return;
    const from = parts[0];
    const to = parts[10].slice(0, -1);
    const change = parseInt(parts[3], 10);
    const happiness = parts[2] === 'lose' ? -change : change;
    if (!happinessMap[from]) happinessMap[from] = {};
    happinessMap[from][to] = happiness;
  });
  return happinessMap;
}

function getGuestList(happinessMap) {
  return Object.keys(happinessMap);
}

function calculateOptimalArrangement(guests, happinessMap) {
  let maxHappiness = 0;
  permute(guests, 0, happinessMap, (arrangement, happiness) => {
    if (happiness > maxHappiness) maxHappiness = happiness;
  });
  return maxHappiness;
}

function permute(arr, i, happinessMap, callback) {
  if (i === arr.length) return;
  if (i === arr.length - 1) {
    const happiness = calculateHappiness(arr, happinessMap);
    callback(arr, happiness);
  }
  for (let j = i; j < arr.length; j++) {
    [arr[i], arr[j]] = [arr[j], arr[i]];
    permute(arr, i + 1, happinessMap, callback);
    [arr[i], arr[j]] = [arr[j], arr[i]];
  }
}

function calculateHappiness(arrangement, happinessMap) {
  let happiness = 0;
  for (let i = 0; i < arrangement.length; i++) {
    const left = (i + arrangement.length - 1) % arrangement.length;
    const right = (i + 1) % arrangement.length;
    happiness += happinessMap[arrangement[i]][arrangement[left]];
    happiness += happinessMap[arrangement[i]][arrangement[right]];
  }
  return happiness;
}