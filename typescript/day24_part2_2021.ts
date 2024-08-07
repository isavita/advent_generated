import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').split('\n');

let k: number[] = [];
let l: number[] = [];
let m: number[] = [];

input.forEach((line, i) => {
  let v: number;
  switch (i % 18) {
    case 4:
      v = Number(line.match(/div z (-?\d+)/)![1]);
      l.push(v);
      break;
    case 5:
      v = Number(line.match(/add x (-?\d+)/)![1]);
      k.push(v);
      break;
    case 15:
      v = Number(line.match(/add y (-?\d+)/)![1]);
      m.push(v);
      break;
  }
});

let constraints: { [key: number]: [number, number] } = {};
let stack: number[] = [];

l.forEach((v, i) => {
  switch (v) {
    case 1:
      stack.push(i);
      break;
    case 26:
      let pop = stack.pop()!;
      constraints[pop] = [i, m[pop] + k[i]];
      break;
  }
});

let min: number[] = new Array(14).fill(0);

for (let i = 0; i < 14; i++) {
  if (!(i in constraints)) continue;
  let vmin = 1;
  while (vmin + constraints[i][1] < 1) vmin++;
  min[i] = vmin;
  min[constraints[i][0]] = vmin + constraints[i][1];
}

console.log(num(min));

function num(w: number[]): number {
  let n = 0;
  for (let i of w) {
    n = n * 10 + i;
  }
  return n;
}