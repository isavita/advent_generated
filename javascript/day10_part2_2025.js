
const fs = require('fs');
const lines = fs.readFileSync('input.txt', 'utf8')
  .split('\n')
  .filter(l => l.trim().length);

let total = 0;

for (const line of lines) {
  const btnRe = /\(([^)]*)\)/g;
  const btns = [];
  let m;
  while ((m = btnRe.exec(line)) !== null) {
    const s = m[1].trim();
    btns.push(s ? s.split(',').map(v => +v.trim()) : []);
  }
  const tgtRe = /\{([^}]*)\}/;
  const tgtMatch = line.match(tgtRe);
  const targets = tgtMatch && tgtMatch[1].trim()
    ? tgtMatch[1].split(',').map(v => +v.trim())
    : [];

  total += solve(btns, targets);
}
console.log(total);

function solve(buttons, targets) {
  const n = targets.length, m = buttons.length;
  const mat = Array.from({ length: n }, () => Array(m + 1).fill(0));
  for (let i = 0; i < n; i++) mat[i][m] = targets[i];
  for (let i = 0; i < m; i++) {
    for (const j of buttons[i]) if (j < n) mat[j][i] = 1;
  }

  const pivCol = Array(n).fill(-1);
  let row = 0;
  for (let col = 0; col < m && row < n; col++) {
    let maxRow = row;
    for (let r = row + 1; r < n; r++)
      if (Math.abs(mat[r][col]) > Math.abs(mat[maxRow][col])) maxRow = r;
    if (Math.abs(mat[maxRow][col]) < 1e-9) continue;
    [mat[row], mat[maxRow]] = [mat[maxRow], mat[row]];
    const scale = mat[row][col];
    for (let c = col; c <= m; c++) mat[row][c] /= scale;
    for (let r = 0; r < n; r++) if (r !== row && Math.abs(mat[r][col]) > 1e-9) {
      const f = mat[r][col];
      for (let c = col; c <= m; c++) mat[r][c] -= f * mat[row][c];
    }
    pivCol[row++] = col;
  }
  const rank = row;
  const isPivot = Array(m).fill(false);
  const pivRow = Array(m).fill(-1);
  for (let r = 0; r < rank; r++) if (pivCol[r] >= 0) {
    isPivot[pivCol[r]] = true;
    pivRow[pivCol[r]] = r;
  }
  const freeVars = [];
  for (let i = 0; i < m; i++) if (!isPivot[i]) freeVars.push(i);

  const maxPresses = Array(m).fill(Number.MAX_SAFE_INTEGER);
  for (let i = 0; i < m; i++) {
    let limit = Number.MAX_SAFE_INTEGER;
    for (const j of buttons[i]) if (j < n && targets[j] < limit) limit = targets[j];
    if (limit === Number.MAX_SAFE_INTEGER) limit = 0;
    maxPresses[i] = limit;
  }
  freeVars.sort((a, b) => maxPresses[a] - maxPresses[b]);

  let best = Number.MAX_SAFE_INTEGER;
  const freeVals = Array(freeVars.length).fill(0);

  function computePivots() {
    const res = Array(m).fill(0);
    for (let i = 0; i < freeVars.length; i++) res[freeVars[i]] = freeVals[i];
    for (let r = rank - 1; r >= 0; r--) {
      const col = pivCol[r];
      if (col < 0) continue;
      let v = mat[r][m];
      for (let c = col + 1; c < m; c++) v -= mat[r][c] * res[c];
      const iv = Math.round(v);
      if (Math.abs(v - iv) > 1e-6 || iv < 0 || iv > maxPresses[col]) return null;
      res[col] = iv;
    }
    return res;
  }

  function enumerate(idx, cur) {
    if (cur >= best) return;
    if (idx === freeVars.length) {
      const arr = computePivots();
      if (arr) {
        const sum = arr.reduce((a, b) => a + b, 0);
        if (sum < best) best = sum;
      }
      return;
    }
    const fv = freeVars[idx];
    const limit = maxPresses[fv];
    for (let v = 0; v <= limit; v++) {
      freeVals[idx] = v;
      enumerate(idx + 1, cur + v);
    }
  }

  enumerate(0, 0);
  return best === Number.MAX_SAFE_INTEGER ? -1 : best;
}
