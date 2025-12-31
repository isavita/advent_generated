
import { readFileSync } from 'fs';

const START = 'you';
const END = 'out';
const lines = readFileSync('input.txt', 'utf8').split('\n');

const adj = new Map<string, string[]>();
for (const raw of lines) {
  const line = raw.trim();
  if (!line) continue;
  const [src, rest] = line.split(':');
  const targets = rest?.trim().split(/\s+/).filter(Boolean) ?? [];
  adj.set(src.trim(), targets);
}

const memo = new Map<string, bigint>();
function count(node: string): bigint {
  if (node === END) return 1n;
  if (memo.has(node)) return memo.get(node)!;
  let total = 0n;
  const neighbors = adj.get(node);
  if (neighbors) {
    for (const nxt of neighbors) total += count(nxt);
  }
  memo.set(node, total);
  return total;
}

console.log(count(START).toString());
