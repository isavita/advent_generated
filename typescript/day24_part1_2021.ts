import * as fs from 'fs';

function main() {
    let k: number[] = [], l: number[] = [], m: number[] = [];
    const lines = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    for (let i = 0; i < lines.length; i++) {
        let v: number;
        switch (i % 18) {
            case 4:
                v = parseInt(lines[i].split(' ')[2]);
                l.push(v);
                break;
            case 5:
                v = parseInt(lines[i].split(' ')[2]);
                k.push(v);
                break;
            case 15:
                v = parseInt(lines[i].split(' ')[2]);
                m.push(v);
                break;
        }
    }

    let constraints: { [key: number]: [number, number] } = {};
    let stack: number[] = [];
    for (let i = 0; i < l.length; i++) {
        switch (l[i]) {
            case 1:
                stack.push(i);
                break;
            case 26:
                let pop = stack.pop()!;
                constraints[pop] = [i, m[pop] + k[i]];
                break;
        }
    }

    let max = Array(14).fill(0);
    for (let i = 0; i < 14; i++) {
        if (!(i in constraints)) continue;
        let vmax = 9;
        while (vmax + constraints[i][1] > 9) vmax--;
        max[i] = vmax;
        max[constraints[i][0]] = vmax + constraints[i][1];
    }

    console.log(num(max));
}

function num(w: number[]): number {
    let n = 0;
    for (let i = 0; i < w.length; i++) {
        n = n * 10 + w[i];
    }
    return n;
}

main();