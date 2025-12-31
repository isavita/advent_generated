
import { readFileSync } from 'fs';

function solveMachine(line: string): number {
  const bStart = line.indexOf('[');
  const bEnd = line.indexOf(']');
  if (bStart === -1 || bEnd === -1) return 0;
  const target = line.slice(bStart + 1, bEnd);
  const m = target.length;
  const b = new Uint8Array(m);
  for (let i = 0; i < m; i++) b[i] = target[i] === '#' ? 1 : 0;

  const btns: Uint8Array[] = [];
  const re = /\((.*?)\)/g;
  let match: RegExpExecArray | null;
  while ((match = re.exec(line))) {
    const vec = new Uint8Array(m);
    for (const s of match[1].split(',')) {
      const t = s.trim();
      if (!t) continue;
      const idx = Number(t);
      if (idx >= 0 && idx < m) vec[idx] = 1;
    }
    btns.push(vec);
  }
  const n = btns.length;
  if (n === 0) return 0;

  const mat: Uint8Array[] = Array.from({ length: m }, (_, i) => {
    const row = new Uint8Array(n + 1);
    for (let j = 0; j < n; j++) row[j] = btns[j][i];
    row[n] = b[i];
    return row;
  });

  const pivotCol = new Int16Array(m).fill(-1);
  let r = 0;
  for (let c = 0; c < n && r < m; c++) {
    let sel = r;
    while (sel < m && mat[sel][c] === 0) sel++;
    if (sel === m) continue;
    [mat[sel], mat[r]] = [mat[r], mat[sel]];
    pivotCol[r] = c;
    for (let i = 0; i < m; i++) {
      if (i !== r && mat[i][c]) {
        for (let j = c; j <= n; j++) mat[i][j] ^= mat[r][j];
      }
    }
    r++;
  }
  for (let i = r; i < m; i++) if (mat[i][n]) return 0;

  const isPivot = new Uint8Array(n);
  for (let i = 0; i < r; i++) isPivot[pivotCol[i]] = 1;
  const free: number[] = [];
  for (let j = 0; j < n; j++) if (!isPivot[j]) free.push(j);
  const f = free.length;
  let best = Number.MAX_SAFE_INTEGER;

  for (let mask = 0; mask < 1 << f; mask++) {
    const x = new Uint8Array(n);
    let w = 0;
    for (let i = 0; i < f; i++) {
      if ((mask >> i) & 1) {
        x[free[i]] = 1;
        w++;
      }
    }
    for (let i = 0; i < r; i++) {
      const pc = pivotCol[i];
      let val = mat[i][n];
      for (let j = 0; j < f; j++) {
        const fj = free[j];
        if (mat[i][fj] && x[fj]) val ^= 1;
      }
      if (val) w++;
    }
    if (w < best) best = w;
  }
  return best === Number.MAX_SAFE_INTEGER ? 0 : best;
}

function main() {
  let total = 0;
  const data = readFileSync('input.txt', 'utf8');
  for (const line of data.split('\n')) {
    const t = line.trim();
    if (t) total += solveMachine(t);
  }
  console.log(total);
}

main();
