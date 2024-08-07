import * as fs from 'fs';

type Direction = 'N' | 'E' | 'S' | 'W';
type Turn = 'L' | 'R';

interface Instruction {
  turn: Turn;
  blocks: number;
}

function parseInstructions(input: string): Instruction[] {
  return input.trim().split(', ').map(instruction => {
    const turn = instruction[0] as Turn;
    const blocks = parseInt(instruction.slice(1), 10);
    return { turn, blocks };
  });
}

function getNewDirection(currentDirection: Direction, turn: Turn): Direction {
  const directions: Direction[] = ['N', 'E', 'S', 'W'];
  const currentIndex = directions.indexOf(currentDirection);
  const turnIndex = turn === 'L' ? -1 : 1;
  return directions[(currentIndex + turnIndex + 4) % 4];
}

function getNewPosition(position: [number, number], direction: Direction, blocks: number): [number, number] {
  const [x, y] = position;
  switch (direction) {
    case 'N': return [x, y + blocks];
    case 'E': return [x + blocks, y];
    case 'S': return [x, y - blocks];
    case 'W': return [x - blocks, y];
  }
}

function calculateDistance(position: [number, number]): number {
  const [x, y] = position;
  return Math.abs(x) + Math.abs(y);
}

function main() {
  const input = fs.readFileSync('input.txt', 'utf-8');
  const instructions = parseInstructions(input);

  let position: [number, number] = [0, 0];
  let direction: Direction = 'N';

  // Part One: Calculate the shortest path to the destination
  for (const { turn, blocks } of instructions) {
    direction = getNewDirection(direction, turn);
    position = getNewPosition(position, direction, blocks);
  }

  const distanceToDestination = calculateDistance(position);
  console.log(`Part One: The shortest path to the destination is ${distanceToDestination} blocks away.`);

  // Part Two: Find the first location visited twice
  position = [0, 0];
  direction = 'N';
  const visited: Set<string> = new Set();
  let firstRepeatedPosition: [number, number] | null = null;

  visited.add(`${position[0]},${position[1]}`);

  for (const { turn, blocks } of instructions) {
    direction = getNewDirection(direction, turn);
    for (let i = 0; i < blocks; i++) {
      position = getNewPosition(position, direction, 1);
      const key = `${position[0]},${position[1]}`;
      if (visited.has(key) && firstRepeatedPosition === null) {
        firstRepeatedPosition = position;
      }
      visited.add(key);
    }
  }

  if (firstRepeatedPosition) {
    const distanceToFirstRepeated = calculateDistance(firstRepeatedPosition);
    console.log(`Part Two: The first location visited twice is ${distanceToFirstRepeated} blocks away.`);
  } else {
    console.log('Part Two: No location was visited twice.');
  }
}

main();