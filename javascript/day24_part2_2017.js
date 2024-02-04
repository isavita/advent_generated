const fs = require('fs');

class Component {
  constructor(a, b) {
    this.a = a;
    this.b = b;
  }
}

let maxStrength = 0;
let maxLength = 0;

function findStrongestLongestBridge(components, used, port, strength, length) {
  if (length > maxLength || (length === maxLength && strength > maxStrength)) {
    maxStrength = strength;
    maxLength = length;
  }

  for (let i = 0; i < components.length; i++) {
    const c = components[i];
    if (used[i]) {
      continue;
    }

    if (c.a === port || c.b === port) {
      used[i] = true;
      const nextPort = c.a === port ? c.b : c.a;
      findStrongestLongestBridge(components, used, nextPort, strength + c.a + c.b, length + 1);
      used[i] = false;
    }
  }
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  const components = data.split('\n').map(line => {
    const ports = line.split('/');
    return new Component(parseInt(ports[0]), parseInt(ports[1]));
  });

  const used = new Array(components.length).fill(false);
  findStrongestLongestBridge(components, used, 0, 0, 0);

  console.log(maxStrength);
});