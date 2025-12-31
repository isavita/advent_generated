
import { readFileSync } from 'fs';

const input = readFileSync('input.txt', 'utf8')
  .split('\n')
  .map(l => l.trim())
  .filter(l => l.length > 0);

let total = 0;

for (const line of input) {
  const buttons: number[][] = [];
  const bm = line.matchAll(/\(([^)]*)\)/g);
  for (const m of bm) {
    const s = m[1].trim();
    if (s === '') {
      buttons.push([]);
      continue;
    }
    buttons.push(s.split(',').map(v => parseInt(v.trim(), 10)));
  }
  const tm = /\{([^}]*)\}/.exec(line);
  if (!tm) continue;
  const targets = tm[1].split(',').map(v => parseInt(v.trim(), 10));
  total += solve(buttons, targets);
}

console.log(total);

function solve(buttons: number[][], targets: number[]): number {
  const n = targets.length;
  const numButtons = buttons.length;
  const matrix: number[][] = Array.from({ length: n }, () => Array(numButtons + 1).fill(0));
  for (let i = 0; i < n; i++) matrix[i][numButtons] = targets[i];
  for (let i = 0; i < numButtons; i++) {
    for (const j of buttons[i]) if (j < n) matrix[j][i] = 1;
  }

  const pivotCol = new Array<number>(n).fill(-1);
  let r = 0;
  for (let c = 0; c < numButtons && r < n; c++) {
    let mr = r;
    for (let i = r + 1; i < n; i++) if (Math.abs(matrix[i][c]) > Math.abs(matrix[mr][c])) mr = i;
    if (Math.abs(matrix[mr][c]) < 1e-9) continue;
    [matrix[r], matrix[mr]] = [matrix[mr], matrix[r]];
    const s = matrix[r][c];
    for (let k = c; k <= numButtons; k++) matrix[r][k] /= s;
    for (let i = 0; i < n; i++) if (i !== r && Math.abs(matrix[i][c]) > 1e-9) {
      const f = matrix[i][c];
      for (let k = c; k <= numButtons; k++) matrix[i][k] -= f * matrix[r][k];
    }
    pivotCol[r++] = c;
  }
  const rank = r;
  for (let i = rank; i < n; i++) if (Math.abs(matrix[i][numButtons]) > 1e-9) return -1;

  const isPivot = new Array<boolean>(numButtons).fill(false);
  for (let i = 0; i < rank; i++) if (pivotCol[i] >= 0) isPivot[pivotCol[i]] = true;
  const freeVars: number[] = [];
  for (let i = 0; i < numButtons; i++) if (!isPivot[i]) freeVars.push(i);

  const maxPresses = new Array<number>(numButtons).fill(0);
  for (let i = 0; i < numButtons; i++) {
    let m = Number.MAX_SAFE_INTEGER;
    for (const j of buttons[i]) if (j < n) m = Math.min(m, targets[j]);
    maxPresses[i] = m === Number.MAX_SAFE_INTEGER ? 0 : m;
  }
  freeVars.sort((a, b) => maxPresses[a] - maxPresses[b]);

  let best = Number.MAX_SAFE_INTEGER;
  const freeVals = new Array<number>(freeVars.length).fill(0);

  function enumerate(idx: number, sum: number) {
    if (sum >= best) return;
    if (idx === freeVars.length) {
      const res = new Array<number>(numButtons).fill(0);
      for (let i = 0; i < freeVars.length; i++) res[freeVars[i]] = freeVals[i];
      for (let i = rank - 1; i >= 0; i--) {
        const c = pivotCol[i];
        if (c < 0) continue;
        let v = matrix[i][numButtons];
        for (let k = c + 1; k < numButtons; k++) v -= matrix[i][k] * res[k];
        const iv = Math.round(v);
        if (Math.abs(v - iv) > 1e-6 || iv < 0 || iv > maxPresses[c]) return;
        res[c] = iv;
      }
      const cur = res.reduce((a, b) => a + b, 0);
      if (cur < best) best = cur;
    } else {
      const fv = freeVars[idx];
      for (let v = 0; v <= maxPresses[fv]; v++) {
        freeVals[idx] = v;
        enumerate(idx + 1, sum + v);
        if (best === 0) return;
      }
    }
  }

  enumerate(0, 0);
  return best === Number.MAX_SAFE_INTEGER ? -1 : best;
}
