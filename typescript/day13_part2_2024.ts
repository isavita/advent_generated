
function solve(): void {
  const offset = 10000000000000;
  const machines = readInput("input.txt");
  const results: number[] = [];
  for (const m of machines) {
    m.px += offset;
    m.py += offset;
    const cost = solveMachine(m);
    if (cost >= 0) {
      results.push(cost);
    }
  }
  if (results.length === 0) {
    console.log("0 0");
    return;
  }
  const count = results.length;
  let sum = 0;
  for (const c of results) {
    sum += c;
  }
  console.log(`${count} ${sum}`);
}

interface Machine {
  ax: number;
  ay: number;
  bx: number;
  by: number;
  px: number;
  py: number;
}

function readInput(filename: string): Machine[] {
  const fs = require("fs");
  const lines = fs.readFileSync(filename, "utf-8").split("\n");
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

function parseLine(s: string): [number, number] {
  const parts = s.trim().split(",");
  return [parseVal(parts[0]), parseVal(parts[1])];
}

function parsePrize(s: string): [number, number] {
  const parts = s.trim().split(",");
  return [parseValPrize(parts[0]), parseValPrize(parts[1])];
}

function parseVal(s: string): number {
  s = s.trim();
  s = s.replace("X+", "").replace("Y+", "").replace("X=", "").replace("Y=", "");
  return parseInt(s, 10);
}

function parseValPrize(s: string): number {
  s = s.trim();
  s = s.replace("X=", "").replace("Y=", "");
  return parseInt(s, 10);
}

function solveMachine(m: Machine): number {
  const D = m.ax * m.by - m.ay * m.bx;
  if (D === 0) {
    return -1;
  }
  const numA = m.px * m.by - m.py * m.bx;
  const numB = -m.px * m.ay + m.py * m.ax;
  if (numA % D !== 0 || numB % D !== 0) {
    return -1;
  }
  const a = numA / D;
  const b = numB / D;
  if (a < 0 || b < 0) {
    return -1;
  }
  return 3 * a + b;
}

solve();
