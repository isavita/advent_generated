function solve(input) {
    const directions = { N: [0, -1], S: [0, 1], E: [1, 0], W: [-1, 0] };
    const map = new Map();
    const stack = [];
    let x = 0, y = 0;
    let maxDoors = 0;
    let roomsOver1000 = 0;

    map.set('0,0', 0);

    function explore(index) {
        while (index < input.length) {
            const char = input[index];
            if (char === '(') {
                stack.push([x, y]);
            } else if (char === ')') {
                [x, y] = stack.pop();
            } else if (char === '|') {
                [x, y] = stack[stack.length - 1];
            } else if (directions[char]) {
                const [dx, dy] = directions[char];
                x += dx;
                y += dy;
                const key = `${x},${y}`;
                const doors = (map.get(key) || Infinity);
                const newDoors = Math.min(doors, map.get(`${x-dx},${y-dy}`) + 1);
                map.set(key, newDoors);
                maxDoors = Math.max(maxDoors, newDoors);
                if (newDoors >= 1000) roomsOver1000++;
            } else if (char === '$') {
                break;
            }
            index++;
        }
        return index;
    }

    explore(1); // Start at index 1 to skip '^'

    return [maxDoors, roomsOver1000];
}

const input = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$";
const [part1, part2] = solve(input);
console.log(`Part 1: ${part1}`);
console.log(`Part 2: ${part2}`);
