const fs = require('fs');

class Component {
  constructor(a, b) {
    this.a = a;
    this.b = b;
  }
}

let maxStrength = 0;

function findStrongestBridge(components, used, port, strength) {
  if (strength > maxStrength) {
    maxStrength = strength;
  }

  components.forEach((c, i) => {
    if (used[i]) {
      return;
    }

    if (c.a === port || c.b === port) {
      used[i] = true;
      const nextPort = c.a === port ? c.b : c.a;
      findStrongestBridge(components, used, nextPort, strength + c.a + c.b);
      used[i] = false;
    }
  });
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  const components = data.trim().split('\n').map(line => {
    const [a, b] = line.split('/').map(Number);
    return new Component(a, b);
  });

  const used = new Array(components.length).fill(false);
  findStrongestBridge(components, used, 0, 0);

  console.log(maxStrength);
});