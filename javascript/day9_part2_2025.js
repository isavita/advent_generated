
const fs = require('fs');
function main() {
  const data = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
  const points = [], uniqX = new Set(), uniqY = new Set();
  for (let line of data) {
    line = line.trim();
    if (!line) continue;
    const [xs, ys] = line.split(',');
    if (xs === undefined || ys === undefined) continue;
    const x = +xs, y = +ys;
    points.push([x, y]);
    uniqX.add(x);
    uniqY.add(y);
  }
  if (!points.length) return console.log('No points found.');
  const xs = [...uniqX].sort((a, b) => a - b);
  const ys = [...uniqY].sort((a, b) => a - b);
  const xIdx = new Map(), yIdx = new Map();
  xs.forEach((v, i) => xIdx.set(v, i));
  ys.forEach((v, i) => yIdx.set(v, i));
  const w = 2 * xs.length + 1, h = 2 * ys.length + 1;
  const colW = new Float64Array(w);
  const rowH = new Float64Array(h);
  colW[0] = 1;
  for (let i = 0; i < xs.length; i++) {
    colW[2 * i + 1] = 1;
    colW[2 * i + 2] = i < xs.length - 1 ? Math.max(0, xs[i + 1] - xs[i] - 1) : 1;
  }
  rowH[0] = 1;
  for (let i = 0; i < ys.length; i++) {
    rowH[2 * i + 1] = 1;
    rowH[2 * i + 2] = i < ys.length - 1 ? Math.max(0, ys[i + 1] - ys[i] - 1) : 1;
  }
  const grid = Array.from({ length: h }, () => new Uint8Array(w));
  const toGrid = ([x, y]) => [2 * xIdx.get(x) + 1, 2 * yIdx.get(y) + 1];
  for (let i = 0; i < points.length; i++) {
    const [gx1, gy1] = toGrid(points[i]);
    const [gx2, gy2] = toGrid(points[(i + 1) % points.length]);
    if (gx1 === gx2) {
      const s = Math.min(gy1, gy2), e = Math.max(gy1, gy2);
      for (let y = s; y <= e; y++) if (rowH[y] > 0) grid[y][gx1] = 1;
    } else {
      const s = Math.min(gx1, gx2), e = Math.max(gx1, gx2);
      for (let x = s; x <= e; x++) if (colW[x] > 0) grid[gy1][x] = 1;
    }
  }
  const qx = [], qy = [];
  qx.push(0); qy.push(0);
  grid[0][0] = 2;
  for (let head = 0; head < qx.length; head++) {
    const x = qx[head], y = qy[head];
    if (x > 0 && grid[y][x - 1] === 0) { grid[y][x - 1] = 2; qx.push(x - 1); qy.push(y); }
    if (x + 1 < w && grid[y][x + 1] === 0) { grid[y][x + 1] = 2; qx.push(x + 1); qy.push(y); }
    if (y > 0 && grid[y - 1][x] === 0) { grid[y - 1][x] = 2; qx.push(x); qy.push(y - 1); }
    if (y + 1 < h && grid[y + 1][x] === 0) { grid[y + 1][x] = 2; qx.push(x); qy.push(y + 1); }
  }
  const pref = Array.from({ length: h }, () => new Float64Array(w));
  for (let y = 0; y < h; y++) {
    let rowSum = 0;
    for (let x = 0; x < w; x++) {
      const val = grid[y][x] !== 2 ? colW[x] * rowH[y] : 0;
      rowSum += val;
      pref[y][x] = rowSum + (y ? pref[y - 1][x] : 0);
    }
  }
  const getSum = (x1, y1, x2, y2) => {
    const lx = Math.min(x1, x2), rx = Math.max(x1, x2);
    const ly = Math.min(y1, y2), ry = Math.max(y1, y2);
    let res = pref[ry][rx];
    if (lx) res -= pref[ry][lx - 1];
    if (ly) res -= pref[ly - 1][rx];
    if (lx && ly) res += pref[ly - 1][lx - 1];
    return res;
  };
  let maxArea = 0;
  for (let i = 0; i < points.length; i++) {
    for (let j = i; j < points.length; j++) {
      const [x1, y1] = points[i];
      const [x2, y2] = points[j];
      const area = (Math.abs(x1 - x2) + 1) * (Math.abs(y1 - y2) + 1);
      if (area <= maxArea) continue;
      const [gx1, gy1] = toGrid(points[i]);
      const [gx2, gy2] = toGrid(points[j]);
      if (getSum(gx1, gy1, gx2, gy2) === area) maxArea = area;
    }
  }
  console.log(`Largest valid area: ${maxArea}`);
}
main();
