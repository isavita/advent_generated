const fs = require('fs');

class Cart {
  constructor(x, y, direction, turns) {
    this.x = x;
    this.y = y;
    this.direction = direction;
    this.turns = turns;
  }
}

function main() {
  const input = fs.readFileSync('input.txt', 'utf8');
  const lines = input.split('\n');

  const tracks = [];
  let carts = [];

  for (let y = 0; y < lines.length; y++) {
    const line = lines[y];
    const trackLine = Array(line.length);
    for (let x = 0; x < line.length; x++) {
      const char = line[x];
      switch (char) {
        case '>':
        case '<':
        case '^':
        case 'v':
          carts.push(new Cart(x, y, char, 0));
          trackLine[x] = char === '>' || char === '<' ? '-' : '|';
          break;
        default:
          trackLine[x] = char;
      }
    }
    tracks.push(trackLine);
  }

  while (carts.length > 1) {
    carts.sort((a, b) => a.y === b.y ? a.x - b.x : a.y - b.y);

    const toRemove = new Set();
    for (let i = 0; i < carts.length; i++) {
      if (toRemove.has(i)) continue;

      const cart = carts[i];
      moveCart(cart, tracks);
      const crashIndex = checkCrash(cart, carts);
      if (crashIndex !== -1) {
        toRemove.add(i);
        toRemove.add(crashIndex);
      }
    }

    carts = carts.filter((_, i) => !toRemove.has(i));
  }

  console.log(`${carts[0].x},${carts[0].y}`);
}

function moveCart(cart, tracks) {
  switch (cart.direction) {
    case '>':
      cart.x++;
      break;
    case '<':
      cart.x--;
      break;
    case '^':
      cart.y--;
      break;
    case 'v':
      cart.y++;
      break;
  }

  switch (tracks[cart.y][cart.x]) {
    case '+':
      turnCart(cart);
      break;
    case '/':
    case '\\':
      changeDirection(cart, tracks[cart.y][cart.x]);
      break;
  }
}

function turnCart(cart) {
  if (cart.turns % 3 === 0) {
    cart.direction = cart.direction === '>' ? '^' :
                     cart.direction === '<' ? 'v' :
                     cart.direction === '^' ? '<' : '>';
  } else if (cart.turns % 3 === 2) {
    cart.direction = cart.direction === '>' ? 'v' :
                     cart.direction === '<' ? '^' :
                     cart.direction === '^' ? '>' : '<';
  }
  cart.turns++;
}

function changeDirection(cart, track) {
  if (track === '/') {
    cart.direction = cart.direction === '>' ? '^' :
                     cart.direction === '<' ? 'v' :
                     cart.direction === '^' ? '>' : '<';
  } else if (track === '\\') {
    cart.direction = cart.direction === '>' ? 'v' :
                     cart.direction === '<' ? '^' :
                     cart.direction === '^' ? '<' : '>';
  }
}

function checkCrash(cart, carts) {
  for (let i = 0; i < carts.length; i++) {
    const c = carts[i];
    if (c !== cart && c.x === cart.x && c.y === cart.y) {
      return i;
    }
  }
  return -1;
}

main();