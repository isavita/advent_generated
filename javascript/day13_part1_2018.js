const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const track = [];
const carts = [];

for(let i = 0; i < input.length; i++) {
  track.push([]);
  for(let j = 0; j < input[i].length; j++) {
    const s = input[i][j];
    switch(s) {
      case '>':
        track[i].push('-');
        carts.push({x: j, y: i, dir: '>', turn: 0});
        break;
      case '<':
        track[i].push('-');
        carts.push({x: j, y: i, dir: '<', turn: 0});
        break;
      case '^':
        track[i].push('|');
        carts.push({x: j, y: i, dir: '^', turn: 0});
        break;
      case 'v':
        track[i].push('|');
        carts.push({x: j, y: i, dir: 'v', turn: 0});
        break;
      default:
        track[i].push(s);
        break;
    }
  }
}

let collision = false;
while(!collision) {
  for(let i = 0; i < carts.length; i++) {
    switch(carts[i].dir) {
      case '>':
        carts[i] = movingRight(track, carts[i]);
        break;
      case '<':
        carts[i] = movingLeft(track, carts[i]);
        break;
      case '^':
        carts[i] = movingUp(track, carts[i]);
        break;
      case 'v':
        carts[i] = movingDown(track, carts[i]);
        break;
      default:
        console.log("error not valid cart");
        break;
    }
  }

  for(let i = 0; i < carts.length; i++) {
    for(let j = i + 1; j < carts.length; j++) {
      if(carts[i].x === carts[j].x && carts[i].y === carts[j].y) {
        collision = true;
        console.log(`${carts[i].x},${carts[i].y}`);
        break;
      }
    }
  }
}

function movingDown(track, cart) {
  switch(track[cart.y + 1][cart.x]) {
    case '/':
      cart.dir = '<';
      break;
    case '\\':
      cart.dir = '>';
      break;
    case '+':
      if(cart.turn === 0) {
        cart.dir = '>';
        cart.turn = 1;
      } else if(cart.turn === 1) {
        cart.turn = 2;
      } else if(cart.turn === 2) {
        cart.dir = '<';
        cart.turn = 0;
      }
      break;
    case '|':
      break;
    default:
      console.log("Error on track cart can't move :", cart.x, cart.y-1, track[cart.y-1][cart.x]);
  }
  cart.y = cart.y + 1;
  return cart;
}

function movingUp(track, cart) {
  switch(track[cart.y - 1][cart.x]) {
    case '/':
      cart.dir = '>';
      break;
    case '\\':
      cart.dir = '<';
      break;
    case '+':
      if(cart.turn === 0) {
        cart.dir = '<';
        cart.turn = 1;
      } else if(cart.turn === 1) {
        cart.turn = 2;
      } else if(cart.turn === 2) {
        cart.dir = '>';
        cart.turn = 0;
      }
      break;
    case '|':
      break;
    default:
      console.log("Error on track cart can't move :", cart.x, cart.y-1, track[cart.y-1][cart.x]);
  }
  cart.y = cart.y - 1;
  return cart;
}

function movingLeft(track, cart) {
  switch(track[cart.y][cart.x - 1]) {
    case '/':
      cart.dir = 'v';
      break;
    case '\\':
      cart.dir = '^';
      break;
    case '+':
      if(cart.turn === 0) {
        cart.dir = 'v';
        cart.turn = 1;
      } else if(cart.turn === 1) {
        cart.turn = 2;
      } else if(cart.turn === 2) {
        cart.dir = '^';
        cart.turn = 0;
      }
      break;
    case '-':
      break;
    default:
      console.log("Error on track cart can't move :", cart.x-1, cart.y, track[cart.y][cart.x-1]);
  }
  cart.x = cart.x - 1;
  return cart;
}

function movingRight(track, cart) {
  switch(track[cart.y][cart.x + 1]) {
    case '\\':
      cart.dir = 'v';
      break;
    case '/':
      cart.dir = '^';
      break;
    case '+':
      if(cart.turn === 0) {
        cart.dir = '^';
        cart.turn = 1;
      } else if(cart.turn === 1) {
        cart.turn = 2;
      } else if(cart.turn === 2) {
        cart.dir = 'v';
        cart.turn = 0;
      }
      break;
    case '-':
      break;
    default:
      console.log("Error on track cart can't move :", cart.x+1, cart.y, track[cart.y][cart.x+1]);
  }
  cart.x = cart.x + 1;
  return cart;
}