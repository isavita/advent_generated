
const fs = require('node:fs');

function parseInput(filePath) {
    const nanobots = [];
    const fileContent = fs.readFileSync(filePath, 'utf-8');
    const lines = fileContent.trim().split('\n');
    const pattern = /pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/;
    for (const line of lines) {
        const match = pattern.exec(line);
        if (match) {
            const [, x, y, z, r] = match.map(Number);
            nanobots.push([x, y, z, r]);
        }
    }
    return nanobots;
}

function manhattanDistance(a, b) {
    return Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]) + Math.abs(a[2] - b[2]);
}

function partOne(nanobots) {
    const strongest = nanobots.reduce((maxBot, bot) => (bot[3] > maxBot[3] ? bot : maxBot));
    const [sx, sy, sz, sr] = strongest;
    let count = 0;
    for (const bot of nanobots) {
        const distance = manhattanDistance([sx, sy, sz], [bot[0], bot[1], bot[2]]);
        if (distance <= sr) {
            count++;
        }
    }
    return count;
}

function minDistanceToOrigin(x, y, z, size) {
    const dx = x > 0 ? x : (x + size - 1 < 0 ? -(x + size - 1) : 0);
    const dy = y > 0 ? y : (y + size - 1 < 0 ? -(y + size - 1) : 0);
    const dz = z > 0 ? z : (z + size - 1 < 0 ? -(z + size - 1) : 0);
    return dx + dy + dz;
}

function partTwo(nanobots) {
    let minX = Infinity, maxX = -Infinity;
    let minY = Infinity, maxY = -Infinity;
    let minZ = Infinity, maxZ = -Infinity;

    for (const bot of nanobots) {
        minX = Math.min(minX, bot[0]);
        maxX = Math.max(maxX, bot[0]);
        minY = Math.min(minY, bot[1]);
        maxY = Math.max(maxY, bot[1]);
        minZ = Math.min(minZ, bot[2]);
        maxZ = Math.max(maxZ, bot[2]);
    }

    let size = 1;
    while (size < Math.max(maxX - minX, maxY - minY, maxZ - minZ)) {
        size *= 2;
    }

    const heap = [[0, minDistanceToOrigin(minX, minY, minZ, size), size, minX, minY, minZ]];

    let bestDistance = null;
    let bestCount = -1;

  const heapify = (arr) => {
    const n = arr.length;
    for (let i = Math.floor(n / 2) - 1; i >= 0; i--) {
      heapifyDown(arr, i, n);
    }
  }

  const heapifyDown = (arr, i, n) => {
        let smallest = i;
        const left = 2 * i + 1;
        const right = 2 * i + 2;

        if (left < n && (arr[left][0] < arr[smallest][0] || (arr[left][0] === arr[smallest][0] && arr[left][1] < arr[smallest][1]))) {
          smallest = left;
        }

        if (right < n && (arr[right][0] < arr[smallest][0] || (arr[right][0] === arr[smallest][0] && arr[right][1] < arr[smallest][1]))) {
          smallest = right;
        }

        if (smallest !== i) {
            [arr[i], arr[smallest]] = [arr[smallest], arr[i]];
            heapifyDown(arr, smallest, n);
        }
  };

    const heappop = (arr) => {
        if (arr.length === 0) return undefined;
        if(arr.length === 1) return arr.pop();
        const top = arr[0];
        arr[0] = arr.pop();
        heapifyDown(arr,0, arr.length);
        return top;

    };

    const heappush = (arr, item) => {
        arr.push(item);
        let i = arr.length -1;
        let parent = Math.floor((i - 1) / 2);

        while(i > 0 && (arr[i][0] < arr[parent][0] || (arr[i][0] === arr[parent][0] && arr[i][1] < arr[parent][1] ))){
            [arr[i], arr[parent]] = [arr[parent], arr[i]];
            i = parent;
            parent = Math.floor((i-1)/2);
        }
    };
    heapify(heap);

    while (heap.length > 0) {
        let [negCount, distance, size, x, y, z] = heappop(heap);
        let count = -negCount;

        if (size === 1) {
            if (count > bestCount || (count === bestCount && distance < bestDistance)) {
                bestCount = count;
                bestDistance = distance;
                break;
            }
            continue;
        }

        const half = Math.floor(size / 2);
        for (const dx of [0, half]) {
            for (const dy of [0, half]) {
                for (const dz of [0, half]) {
                    const nx = x + dx;
                    const ny = y + dy;
                    const nz = z + dz;
                    let newSize = half;
                    if (newSize < 1) {
                        newSize = 1;
                    }

                    let count = 0;
                    for (const bot of nanobots) {
                        const [bx, by, bz, br] = bot;
                        let d = 0;
                        if (bx < nx) {
                            d += nx - bx;
                        } else if (bx > nx + newSize - 1) {
                            d += bx - (nx + newSize - 1);
                        }
                        if (by < ny) {
                            d += ny - by;
                        } else if (by > ny + newSize - 1) {
                            d += by - (ny + newSize - 1);
                        }
                        if (bz < nz) {
                            d += nz - bz;
                        } else if (bz > nz + newSize - 1) {
                            d += bz - (nz + newSize - 1);
                        }
                        if (d <= br) {
                            count++;
                        }
                    }

                    const newDistance = minDistanceToOrigin(nx, ny, nz, newSize);
                  heappush(heap, [-count, newDistance, newSize, nx, ny, nz]);

                }
            }
        }
    }

    return bestDistance;
}

function main() {
    const inputFile = 'input.txt';
    const nanobots = parseInput(inputFile);

    const countInRange = partOne(nanobots);
    console.log(countInRange);

    const shortestDistance = partTwo(nanobots);
    console.log(shortestDistance);
}

main();
