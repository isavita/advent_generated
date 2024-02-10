const fs = require('fs');

function countOrbits(orbitMap, start, depth) {
  const orbits = orbitMap[start];
  if (!orbits) {
    return depth;
  }
  let count = depth;
  for (const orbit of orbits) {
    count += countOrbits(orbitMap, orbit, depth + 1);
  }
  return count;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  const lines = data.trim().split('\n');
  const orbitMap = {};
  for (const line of lines) {
    const [center, orbiter] = line.split(')');
    if (!orbitMap[center]) {
      orbitMap[center] = [];
    }
    orbitMap[center].push(orbiter);
  }

  const totalOrbits = countOrbits(orbitMap, 'COM', 0);
  console.log(totalOrbits);
});