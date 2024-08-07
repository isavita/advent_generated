import * as fs from 'fs';

interface Cart {
    x: number;
    y: number;
    direction: string;
    turnCount: number;
}

const input = fs.readFileSync('input.txt', 'utf-8').split('\n');
const directions = ['^', '>', 'v', '<'];
const turns = ['left', 'straight', 'right'];
const trackMap: string[][] = [];
const carts: Cart[] = [];

input.forEach((line, y) => {
    const row = line.split('').map(char => char);
    trackMap.push(row);
    row.forEach((char, x) => {
        if (directions.includes(char)) {
            carts.push({ x, y, direction: char, turnCount: 0 });
            trackMap[y][x] = char === '^' || char === 'v' ? '|' : '-';
        }
    });
});

const moveCart = (cart: Cart) => {
    const { x, y, direction } = cart;
    let newX = x;
    let newY = y;

    switch (direction) {
        case '^': newY--; break;
        case 'v': newY++; break;
        case '<': newX--; break;
        case '>': newX++; break;
    }

    const track = trackMap[newY][newX];

    if (track === '+') {
        const turn = turns[cart.turnCount % 3];
        cart.turnCount++;
        if (turn === 'left') {
            cart.direction = directions[(directions.indexOf(direction) + 3) % 4];
        } else if (turn === 'right') {
            cart.direction = directions[(directions.indexOf(direction) + 1) % 4];
        }
    } else if (track === '/') {
        cart.direction = direction === '^' ? '>' : direction === '>' ? '^' : direction === 'v' ? '<' : 'v';
    } else if (track === '\\') {
        cart.direction = direction === '^' ? '<' : direction === '>' ? 'v' : direction === 'v' ? '>' : '^';
    }

    cart.x = newX;
    cart.y = newY;
};

const findFirstCrash = () => {
    const positions = new Set<string>();
    while (true) {
        carts.sort((a, b) => a.y - b.y || a.x - b.x);
        const newPositions = new Set<string>();

        for (const cart of carts) {
            const posKey = `${cart.x},${cart.y}`;
            if (newPositions.has(posKey)) {
                return posKey; // Collision found
            }
            newPositions.add(posKey);
            moveCart(cart);
        }
    }
};

const firstCrash = findFirstCrash();
console.log(`First crash at: ${firstCrash}`);