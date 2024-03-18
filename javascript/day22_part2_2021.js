const fs = require('fs');

function part1OutOfBounds(...nums) {
  for (const n of nums) {
    if (n < -50 || n > 50) {
      return true;
    }
  }
  return false;
}

function parseInput(input) {
  const ans = [];
  for (const line of input.trim().split('\n')) {
    const parts = line.split(' ');
    let [x1, x2, y1, y2, z1, z2] = parts[1].split(',').map(part => {
      const [_, value] = part.split('=');
      const [min, max] = value.split('..');
      return [parseInt(min), parseInt(max)];
    }).flat();

    if (x1 > x2 || y1 > y2 || z1 > z2) {
      throw new Error('Didn\'t expect input to have backwards coords, sort them...');
    }

    ans.push({
      isOn: parts[0] === 'on',
      x1,
      x2,
      y1,
      y2,
      z1,
      z2,
    });
  }
  return ans;
}

function getIntersection(c1, c2) {
  const x1 = Math.max(c1.x1, c2.x1);
  const x2 = Math.min(c1.x2, c2.x2);
  const y1 = Math.max(c1.y1, c2.y1);
  const y2 = Math.min(c1.y2, c2.y2);
  const z1 = Math.max(c1.z1, c2.z1);
  const z2 = Math.min(c1.z2, c2.z2);

  if (x1 > x2 || y1 > y2 || z1 > z2) {
    return { hasIntersection: false };
  }

  let intersectionState;
  if (c1.isOn && c2.isOn) {
    intersectionState = false;
  } else if (!c1.isOn && !c2.isOn) {
    intersectionState = true;
  } else {
    intersectionState = c2.isOn;
  }

  return {
    hasIntersection: true,
    intersection: {
      isOn: intersectionState,
      x1,
      x2,
      y1,
      y2,
      z1,
      z2,
    },
  };
}

function volume(c) {
  const vol = (c.x2 - c.x1 + 1) * (c.y2 - c.y1 + 1) * (c.z2 - c.z1 + 1);
  return c.isOn ? vol : -vol;
}

function solve(input) {
  const cubes = parseInput(input);

  const finalList = [];
  for (const c of cubes) {
    const toAdd = [];
    for (const finalCube of finalList) {
      const { hasIntersection, intersection } = getIntersection(finalCube, c);
      if (hasIntersection) {
        toAdd.push(intersection);
      }
    }

    if (c.isOn) {
      toAdd.push(c);
    }

    finalList.push(...toAdd);
  }

  let total = 0;
  for (const c of finalList) {
    total += volume(c);
  }

  return total;
}

const input = fs.readFileSync('input.txt', 'utf-8');
const result = solve(input);
console.log(result);