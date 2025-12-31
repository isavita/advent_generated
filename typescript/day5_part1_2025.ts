
import { readFileSync } from 'fs';

type Range = { start: number; end: number };

function merge(ranges: Range[]): Range[] {
  if (!ranges.length) return [];
  ranges.sort((a, b) => a.start - b.start);
  const merged: Range[] = [];
  let cur = ranges[0];
  for (let i = 1; i < ranges.length; i++) {
    const nxt = ranges[i];
    if (nxt.start <= cur.end + 1) cur.end = Math.max(cur.end, nxt.end);
    else {
      merged.push(cur);
      cur = nxt;
    }
  }
  merged.push(cur);
  return merged;
}

function contains(id: number, ranges: Range[]): boolean {
  let l = 0,
    r = ranges.length - 1;
  while (l <= r) {
    const m = (l + r) >> 1;
    if (id < ranges[m].start) r = m - 1;
    else if (id > ranges[m].end) l = m + 1;
    else return true;
  }
  return false;
}

function main() {
  const data = readFileSync('input.txt', 'utf8').split(/\r?\n/);
  const ranges: Range[] = [];
  let i = 0;
  for (; i < data.length; i++) {
    const line = data[i].trim();
    if (!line) break;
    const [a, b] = line.split('-').map(Number);
    ranges.push({ start: a, end: b });
  }
  const ids: number[] = [];
  for (i = i + 1; i < data.length; i++) {
    const parts = data[i].trim().split(/\s+/);
    for (const p of parts) if (p) ids.push(Number(p));
  }
  const merged = merge(ranges);
  let count = 0;
  for (const id of ids) if (contains(id, merged)) count++;
  console.log(count);
}

main();
