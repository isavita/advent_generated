const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
console.log(visited(input, 10));

function visited(input, ropelen) {
  const rope = Array.from({ length: ropelen }, () => ({ x: 0, y: 0 }));
  const visited = new Set();
  input.split('\n').forEach(line => {
    const [b, n] = line.split(' ');
    const d = dirFromByte(b);
    for (let i = 0; i < parseInt(n); i++) {
      rope[0].x += d.x;
      rope[0].y += d.y;
      for (let j = 1; j < ropelen; j++) {
        rope[j] = next(rope[j - 1], rope[j]);
      }
      visited.add(`${rope[ropelen - 1].x},${rope[ropelen - 1].y}`);
    }
  });
  return visited.size;
}

function next(head, tail) {
  if (Math.abs(head.x - tail.x) <= 1 && Math.abs(head.y - tail.y) <= 1) {
    return tail;
  }
  return {
    x: tail.x + Math.sign(head.x - tail.x),
    y: tail.y + Math.sign(head.y - tail.y)
  };
}

function dirFromByte(b) {
  const byteMap = {
    'N': { x: 0, y: 1 },
    'E': { x: 1, y: 0 },
    'S': { x: 0, y: -1 },
    'W': { x: -1, y: 0 },
    'U': { x: 0, y: 1 },
    'R': { x: 1, y: 0 },
    'D': { x: 0, y: -1 },
    'L': { x: -1, y: 0 },
    '^': { x: 0, y: 1 },
    '>': { x: 1, y: 0 },
    'v': { x: 0, y: -1 },
    '<': { x: -1, y: 0 },
  };
  return byteMap[b];
}