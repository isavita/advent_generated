const fs = require('fs');

interface Component {
  a: number;
  b: number;
}

let maxStrength: number = 0;
let maxLength: number = 0;

function findStrongestLongestBridge(components: Component[], used: boolean[], port: number, strength: number, length: number) {
  if (length > maxLength || (length === maxLength && strength > maxStrength)) {
    maxStrength = strength;
    maxLength = length;
  }

  for (let i = 0; i < components.length; i++) {
    if (used[i]) {
      continue;
    }

    if (components[i].a === port || components[i].b === port) {
      used[i] = true;
      let nextPort = components[i].a;
      if (components[i].a === port) {
        nextPort = components[i].b;
      }
      findStrongestLongestBridge(components, used, nextPort, strength + components[i].a + components[i].b, length + 1);
      used[i] = false;
    }
  }
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const components: Component[] = input.map((line: string) => {
  const [a, b] = line.split('/');
  return { a: parseInt(a), b: parseInt(b) };
});

const used: boolean[] = new Array(components.length).fill(false);
findStrongestLongestBridge(components, used, 0, 0, 0);

console.log(maxStrength);