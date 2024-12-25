
interface Machine {
  ax: number;
  ay: number;
  bx: number;
  by: number;
  px: number;
  py: number;
}

function solveMachine(m: Machine): number {
  if (m.px === 0 && m.py === 0) return 0;
  if (m.ax === 0 && m.ay === 0) {
    if (m.bx === 0 && m.by === 0) return -1;
    if (m.bx === 0) {
      if (m.px !== 0) return -1;
      const bCount = m.py / m.by;
      if (bCount === Math.floor(bCount) && bCount >= 0) return bCount;
      return -1;
    }
    if (m.by === 0) {
      if (m.py !== 0) return -1;
      const bCount = m.px / m.bx;
      if (bCount === Math.floor(bCount) && bCount >= 0) return bCount;
      return -1;
    }
    const bCountX = m.px / m.bx;
    const bCountY = m.py / m.by;
    if (bCountX === Math.floor(bCountX) && bCountX >= 0 && bCountY === Math.floor(bCountY) && bCountY >= 0 && bCountX === bCountY) return bCountX;
    return -1;
  }
  if (m.bx === 0 && m.by === 0) {
    if (m.ax === 0) {
      if (m.px !== 0) return -1;
      const aCount = m.py / m.ay;
      if (aCount === Math.floor(aCount) && aCount >= 0) return aCount * 3;
      return -1;
    }
    if (m.ay === 0) {
      if (m.py !== 0) return -1;
      const aCount = m.px / m.ax;
      if (aCount === Math.floor(aCount) && aCount >= 0) return aCount * 3;
      return -1;
    }
    const aCountX = m.px / m.ax;
    const aCountY = m.py / m.ay;
    if (aCountX === Math.floor(aCountX) && aCountX >= 0 && aCountY === Math.floor(aCountY) && aCountY >= 0 && aCountX === aCountY) return aCountX * 3;
    return -1;
  }

  let minCost = -1;
  for (let aCount = 0; aCount <= 100; aCount++) {
    const x = m.px - m.ax * aCount;
    const y = m.py - m.ay * aCount;

    if (m.bx === 0) {
      if (x !== 0) continue;
      if (m.by === 0) continue;
      const bCount = y / m.by;
      if (bCount === Math.floor(bCount) && bCount >= 0) {
        const cost = aCount * 3 + bCount;
        if (minCost < 0 || cost < minCost) minCost = cost;
      }
      continue;
    }

    if (m.by === 0) {
      if (y !== 0) continue;
      const bCount = x / m.bx;
      if (bCount === Math.floor(bCount) && bCount >= 0) {
        const cost = aCount * 3 + bCount;
        if (minCost < 0 || cost < minCost) minCost = cost;
      }
      continue;
    }

    if ((x / m.bx) !== (y / m.by)) continue;
    const bCount = x / m.bx;

    if (bCount === Math.floor(bCount) && bCount >= 0) {
      const cost = aCount * 3 + bCount;
      if (minCost < 0 || cost < minCost) minCost = cost;
    }
  }
  return minCost;
}

function parseLine(s: string): [number, number] {
  const parts = s.trim().split(",");
  const xp = parts[0].trim();
  const yp = parts[1].trim();
  const x = parseVal(xp);
  const y = parseVal(yp);
  return [x, y];
}

function parsePrize(s: string): [number, number] {
  const parts = s.trim().split(",");
  const xp = parts[0].trim();
  const yp = parts[1].trim();
  const x = parseValPrize(xp);
  const y = parseValPrize(yp);
  return [x, y];
}

function parseVal(s: string): number {
  s = s.trim();
  s = s.startsWith("X+") ? s.substring(2) : s;
  s = s.startsWith("Y+") ? s.substring(2) : s;
  s = s.startsWith("X=") ? s.substring(2) : s;
  s = s.startsWith("Y=") ? s.substring(2) : s;
  return parseInt(s);
}

function parseValPrize(s: string): number {
  s = s.trim();
  s = s.startsWith("X=") ? s.substring(2) : s;
  s = s.startsWith("Y=") ? s.substring(2) : s;
  return parseInt(s);
}

function parseMachine(lines: string[]): Machine {
  const m: Machine = { ax: 0, ay: 0, bx: 0, by: 0, px: 0, py: 0 };
  for (const l of lines) {
    const line = l.replace("Button A:", "A:").replace("Button B:", "B:").replace("Prize:", "P:");
    if (line.startsWith("A:")) {
      [m.ax, m.ay] = parseLine(line.substring(2));
    } else if (line.startsWith("B:")) {
      [m.bx, m.by] = parseLine(line.substring(2));
    } else if (line.startsWith("P:")) {
      [m.px, m.py] = parsePrize(line.substring(2));
    }
  }
  return m;
}

function readInput(filename: string): Machine[] {
  const fs = require("fs");
  const data = fs.readFileSync(filename, "utf-8");
  const lines = data.split("\n");
  const machines: Machine[] = [];
  let currentLines: string[] = [];
  for (const line of lines) {
    const trimmedLine = line.trim();
    if (trimmedLine === "") {
      if (currentLines.length > 0) {
        machines.push(parseMachine(currentLines));
        currentLines = [];
      }
    } else {
      currentLines.push(trimmedLine);
    }
  }
  if (currentLines.length > 0) {
    machines.push(parseMachine(currentLines));
  }
  return machines;
}

const machines = readInput("input.txt");
const results: number[] = [];
for (const m of machines) {
  const cost = solveMachine(m);
  if (cost >= 0) {
    results.push(cost);
  }
}
if (results.length === 0) {
  console.log("0 0");
} else {
  const count = results.length;
  let sum = 0;
  for (const c of results) {
    sum += c;
  }
  console.log(`${count} ${sum}`);
}
