
const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

function bitCount(v) {
  let c = 0;
  while (v) {
    c += v & 1;
    v >>>= 1;
  }
  return c;
}

function minWeight(mat, R, C) {
  const m = mat.map(row => row.slice());
  const colPivot = new Array(C).fill(false);
  let piv = 0;
  for (let c = 0; c < C && piv < R; ++c) {
    let sel = -1;
    for (let r = piv; r < R && sel === -1; ++r) if (m[r][c] === 1) sel = r;
    if (sel !== -1) {
      [m[piv], m[sel]] = [m[sel], m[piv]];
      for (let r = 0; r < R; ++r) if (r !== piv && m[r][c] === 1) {
        for (let k = c; k <= C; ++k) m[r][k] ^= m[piv][k];
      }
      colPivot[c] = true;
      ++piv;
    }
  }
  for (let r = piv; r < R; ++r) if (m[r][C] === 1) return -1;
  const free = [];
  for (let c = 0; c < C; ++c) if (!colPivot[c]) free.push(c);
  const nFree = free.length;
  const limit = 1 << nFree;
  const x = new Array(C).fill(0);
  let best = Number.MAX_SAFE_INTEGER;
  for (let mask = 0; mask < limit; ++mask) {
    x.fill(0);
    let w = bitCount(mask);
    for (let j = 0; j < nFree; ++j) if ((mask >> j) & 1) x[free[j]] = 1;
    let prow = 0;
    for (let c = 0; c < C; ++c) if (colPivot[c]) {
      let v = m[prow][C];
      for (let k = c + 1; k < C; ++k) if (m[prow][k] === 1) v ^= x[k];
      x[c] = v;
      if (v) ++w;
      ++prow;
    }
    if (w < best) best = w;
  }
  return best;
}

let total = 0;
const btnPat = /\(([^)]*)\)/g;

for (let raw of input) {
  const line = raw.trim();
  const lb = line.indexOf('[');
  const rb = line.indexOf(']', lb);
  if (lb === -1 || rb === -1) continue;
  const targetStr = line.slice(lb + 1, rb);
  const R = targetStr.length;
  const target = Array.from(targetStr, ch => (ch === '#' ? 1 : 0));
  const btnMatches = [...line.slice(rb + 1).matchAll(btnPat)];
  const buttons = btnMatches.map(m => {
    const s = m[1].trim();
    return s ? s.split(',').map(v => parseInt(v, 10)) : [];
  });
  const C = buttons.length;
  const matrix = Array.from({ length: R }, () => new Array(C + 1).fill(0));
  for (let r = 0; r < R; ++r) {
    for (let c = 0; c < C; ++c) matrix[r][c] = buttons[c].includes(r) ? 1 : 0;
    matrix[r][C] = target[r];
  }
  const mw = minWeight(matrix, R, C);
  if (mw !== -1) total += mw;
}

console.log(total);
