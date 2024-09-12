function countWaterTiles(input) {
  const clay = new Set();
  let minY = Infinity, maxY = -Infinity;

  // Parse input
  input.split('\n').forEach(line => {
    const [a, b] = line.split(', ');
    const [axis1, value1] = a.split('=');
    const [axis2, range] = b.split('=');
    const [start, end] = range.split('..').map(Number);

    for (let i = start; i <= end; i++) {
      const x = axis1 === 'x' ? Number(value1) : i;
      const y = axis1 === 'y' ? Number(value1) : i;
      clay.add(`${x},${y}`);
      minY = Math.min(minY, y);
      maxY = Math.max(maxY, y);
    }
  });

  const water = new Set();
  const flowing = new Set();

  function pour(x, y) {
    if (y > maxY) return false;
    if (water.has(`${x},${y}`) || clay.has(`${x},${y}`)) return true;
    if (flowing.has(`${x},${y}`)) return false;

    flowing.add(`${x},${y}`);

    if (!pour(x, y + 1)) return false;

    let left = pour(x - 1, y);
    let right = pour(x + 1, y);

    if (left && right) {
      for (let i = x; !clay.has(`${i},${y}`); i--) water.add(`${i},${y}`);
      for (let i = x + 1; !clay.has(`${i},${y}`); i++) water.add(`${i},${y}`);
      return true;
    }

    return false;
  }

  pour(500, 0);

  let count = 0;
  for (let y = minY; y <= maxY; y++) {
    for (let x = 0; x < 1000; x++) {
      if (water.has(`${x},${y}`) || flowing.has(`${x},${y}`)) count++;
    }
  }

  return count;
}

// Example usage:
const input = `x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504`;

console.log(countWaterTiles(input));
