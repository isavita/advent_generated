import * as fs from 'fs';
import * as crypto from 'crypto';

type Point = [number, number];

const DELTAS: [Point, string][] = [
  [[0, -1], 'U'],
  [[0, 1], 'D'],
  [[-1, 0], 'L'],
  [[1, 0], 'R'],
];

const isOpen = (char: string): boolean => 'bcdef'.includes(char);

const hash = (input: string): string =>
  crypto.createHash('md5').update(input).digest('hex');

const isValid = (x: number, y: number): boolean =>
  x >= 0 && x < 4 && y >= 0 && y < 4;

const bfs = (passcode: string): string | null => {
  const queue: [Point, string][] = [[[0, 0], '']];
  const visited = new Set<string>();

  while (queue.length > 0) {
    const [current, path] = queue.shift()!;
    const [x, y] = current;

    if (x === 3 && y === 3) {
      return path;
    }

    const currentHash = hash(passcode + path);
    for (let i = 0; i < 4; i++) {
      const [dx, dy] = DELTAS[i][0];
      const newX = x + dx;
      const newY = y + dy;
      const newPath = path + DELTAS[i][1];

      if (isValid(newX, newY) && isOpen(currentHash[i]) && !visited.has(newPath)) {
        visited.add(newPath);
        queue.push([[newX, newY], newPath]);
      }
    }
  }

  return null;
};

const main = () => {
  const input = fs.readFileSync('input.txt', 'utf-8').trim();
  const shortestPath = bfs(input);

  if (shortestPath) {
    console.log(shortestPath);
  } else {
    console.log('No path found.');
  }
};

main();