import * as fs from 'fs';

interface Portal {
  name: string;
  isOuter: boolean;
}

interface QueueEntry {
  pos: string;
  level: number;
  steps: number;
}

function parseMaze(input: string[]): { maze: Map<string, string>, portals: Map<string, Portal> } {
  const maze = new Map<string, string>();
  const portals = new Map<string, Portal>();
  const width = Math.max(...input.map(line => line.length));
  const height = input.length;

  // Read maze into 2D grid
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const char = input[y][x] || ' ';
      if (char !== ' ') {
        maze.set(`${x},${y}`, char);
      }
    }
  }

  // Find portals
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const currentChar = input[y]?.[x] || '';
      if (/[A-Z]/.test(currentChar)) {
        // Check horizontal portal
        if (/[A-Z]/.test(input[y]?.[x + 1] || '')) {
          const name = input[y][x] + input[y][x + 1];
          const isOuter = x === 0 || x + 2 >= width - 2;

          if (maze.get(`${x - 1},${y}`) === '.') {
            portals.set(`${x - 1},${y}`, { name, isOuter });
          } else if (maze.get(`${x + 2},${y}`) === '.') {
            portals.set(`${x + 2},${y}`, { name, isOuter });
          }
        }
        // Check vertical portal
        if (/[A-Z]/.test(input[y + 1]?.[x] || '')) {
          const name = input[y][x] + input[y + 1][x];
          const isOuter = y === 0 || y + 2 >= height - 2;

          if (maze.get(`${x},${y - 1}`) === '.') {
            portals.set(`${x},${y - 1}`, { name, isOuter });
          } else if (maze.get(`${x},${y + 2}`) === '.') {
            portals.set(`${x},${y + 2}`, { name, isOuter });
          }
        }
      }
    }
  }

  return { maze, portals };
}

function findShortestPath(maze: Map<string, string>, portals: Map<string, Portal>): number {
  const startEntry = [...portals.entries()].find(([_, p]) => p.name === 'AA');
  const endEntry = [...portals.entries()].find(([_, p]) => p.name === 'ZZ');

  if (!startEntry || !endEntry) {
    throw new Error('Start or end portal not found');
  }

  const start = startEntry[0];
  const end = endEntry[0];

  const portalsByName = new Map<string, Array<{ pos: string; isOuter: boolean }>>();
  for (const [pos, portal] of portals.entries()) {
    if (!portalsByName.has(portal.name)) {
      portalsByName.set(portal.name, []);
    }
    portalsByName.get(portal.name)!.push({ pos, isOuter: portal.isOuter });
  }

  const queue: QueueEntry[] = [{ pos: start, level: 0, steps: 0 }];
  const seen = new Set<string>();
  const dirs: [number, number][] = [[0, 1], [1, 0], [0, -1], [-1, 0]];

  while (queue.length > 0) {
    const { pos, level, steps } = queue.shift()!;
    const key = `${pos},${level}`;

    if (seen.has(key)) continue;
    seen.add(key);

    // Found the exit at level 0
    if (pos === end && level === 0) {
      return steps;
    }

    // Try normal moves
    const [x, y] = pos.split(',').map(Number);
    for (const [dx, dy] of dirs) {
      const newPos = `${x + dx},${y + dy}`;
      if (maze.get(newPos) === '.') {
        queue.push({ pos: newPos, level, steps: steps + 1 });
      }
    }

    // Try portal moves
    if (portals.has(pos)) {
      const portal = portals.get(pos)!;
      if (portal.name !== 'AA' && portal.name !== 'ZZ') {
        const candidates = portalsByName.get(portal.name)!;
        const others = candidates.filter(p => p.pos !== pos);
        if (others.length > 0) {
          const other = others[0];
          // Outer portals decrease level, inner portals increase level
          const newLevel = level + (portal.isOuter ? -1 : 1);

          // Only use portal if we don't go below level 0
          if (newLevel >= 0) {
            queue.push({ pos: other.pos, level: newLevel, steps: steps + 1 });
          }
        }
      }
    }
  }

  return -1; // No path found
}

// Main execution
const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const { maze, portals } = parseMaze(input);
const steps = findShortestPath(maze, portals);
console.log(steps);
