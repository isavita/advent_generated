import * as fs from 'fs';

interface Cart {
  x: number;
  y: number;
  direction: string;
  turns: number;
}

interface Position {
  x: number;
  y: number;
}

function main() {
  const file = fs.readFileSync('input.txt', 'utf8');
  const tracks: string[][] = [];
  let carts: Cart[] = [];

  file.split('\n').forEach((line, y) => {
    const trackLine = line.split('');
    trackLine.forEach((r, x) => {
      switch (r) {
        case '>':
        case '<':
        case '^':
        case 'v':
          carts.push({ x, y, direction: r, turns: 0 });
          if (r === '>' || r === '<') {
            trackLine[x] = '-';
          } else {
            trackLine[x] = '|';
          }
          break;
      }
    });
    tracks.push(trackLine);
  });

  while (carts.length > 1) {
    carts.sort((a, b) => a.y === b.y ? a.x - b.x : a.y - b.y);

    const toRemove: Set<number> = new Set();
    carts.forEach((cart, i) => {
      if (toRemove.has(i)) return;

      moveCart(cart, tracks);
      const crashIndex = checkCrash(cart, carts);
      if (crashIndex !== -1) {
        toRemove.add(i);
        toRemove.add(crashIndex);
      }
    });

    carts = carts.filter((_, i) => !toRemove.has(i));
  }

  console.log(`${carts[0].x},${carts[0].y}`);
}

function moveCart(cart: Cart, tracks: string[][]) {
  switch (cart.direction) {
    case '>': cart.x++; break;
    case '<': cart.x--; break;
    case '^': cart.y--; break;
    case 'v': cart.y++; break;
  }

  switch (tracks[cart.y][cart.x]) {
    case '+': turnCart(cart); break;
    case '/':
    case '\\': changeDirection(cart, tracks[cart.y][cart.x]); break;
  }
}

function turnCart(cart: Cart) {
  if (cart.turns % 3 === 0) {
    if (cart.direction === '>') cart.direction = '^';
    else if (cart.direction === '<') cart.direction = 'v';
    else if (cart.direction === '^') cart.direction = '<';
    else cart.direction = '>';
  } else if (cart.turns % 3 === 2) {
    if (cart.direction === '>') cart.direction = 'v';
    else if (cart.direction === '<') cart.direction = '^';
    else if (cart.direction === '^') cart.direction = '>';
    else cart.direction = '<';
  }
  cart.turns++;
}

function changeDirection(cart: Cart, track: string) {
  if (track === '/') {
    if (cart.direction === '>') cart.direction = '^';
    else if (cart.direction === '<') cart.direction = 'v';
    else if (cart.direction === '^') cart.direction = '>';
    else cart.direction = '<';
  } else if (track === '\\') {
    if (cart.direction === '>') cart.direction = 'v';
    else if (cart.direction === '<') cart.direction = '^';
    else if (cart.direction === '^') cart.direction = '<';
    else cart.direction = '>';
  }
}

function checkCrash(cart: Cart, carts: Cart[]): number {
  for (let i = 0; i < carts.length; i++) {
    const c = carts[i];
    if (c !== cart && c.x === cart.x && c.y === cart.y) {
      return i;
    }
  }
  return -1;
}

main();