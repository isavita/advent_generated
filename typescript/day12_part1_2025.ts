
import { readFileSync } from 'fs';

interface Pt { x: number; y: number }
interface Ori { points: Pt[]; w: number; h: number }
class Shape {
    orientations: Ori[];
    area: number;
    constructor(rows: string[]) {
        const pts: Pt[] = [];
        rows.forEach((r, y) => {
            for (let x = 0; x < r.length; x++) if (r[x] === '#') pts.push({ x, y });
        });
        this.area = pts.length;
        this.orientations = this.genOris(pts);
    }
    private genOris(base: Pt[]): Ori[] {
        const uniq = new Set<string>();
        const res: Ori[] = [];
        for (let i = 0; i < 8; i++) {
            const tr: Pt[] = base.map(p => {
                let nx = 0, ny = 0;
                switch (i) {
                    case 0: nx = p.x; ny = p.y; break;
                    case 1: nx = p.y; ny = -p.x; break;
                    case 2: nx = -p.x; ny = -p.y; break;
                    case 3: nx = -p.y; ny = p.x; break;
                    case 4: nx = -p.x; ny = p.y; break;
                    case 5: nx = p.y; ny = p.x; break;
                    case 6: nx = p.x; ny = -p.y; break;
                    case 7: nx = -p.y; ny = -p.x; break;
                }
                return { x: nx, y: ny };
            });
            let minX = Infinity, minY = Infinity;
            tr.forEach(p => { if (p.x < minX) minX = p.x; if (p.y < minY) minY = p.y; });
            tr.forEach(p => { p.x -= minX; p.y -= minY; });
            tr.sort((a, b) => a.y !== b.y ? a.y - b.y : a.x - b.x);
            const key = tr.map(p => `${p.x},${p.y}`).join(';');
            if (uniq.has(key)) continue;
            uniq.add(key);
            let w = 0, h = 0;
            tr.forEach(p => { if (p.x > w) w = p.x; if (p.y > h) h = p.y; });
            res.push({ points: tr, w: w + 1, h: h + 1 });
        }
        return res;
    }
}

function solve(idx: number, grid: boolean[], W: number, H: number, shapes: Shape[], remArea: number, freeArea: number): boolean {
    if (idx === shapes.length) return true;
    if (remArea > freeArea) return false;
    const shape = shapes[idx];
    for (const o of shape.orientations) {
        if (o.w > W || o.h > H) continue;
        for (let r = 0; r <= H - o.h; r++) {
            outer: for (let c = 0; c <= W - o.w; c++) {
                for (const p of o.points) {
                    if (grid[(r + p.y) * W + (c + p.x)]) continue outer;
                }
                for (const p of o.points) grid[(r + p.y) * W + (c + p.x)] = true;
                if (solve(idx + 1, grid, W, H, shapes, remArea - shape.area, freeArea - shape.area)) return true;
                for (const p of o.points) grid[(r + p.y) * W + (c + p.x)] = false;
            }
        }
    }
    return false;
}

function handleRegion(line: string, allShapes: Shape[], inc: (v: number) => void) {
    const [dimPart, cntPart] = line.split(':');
    const [W, H] = dimPart.split('x').map(Number);
    const counts = cntPart.trim().split(/\s+/).map(Number);
    const toFit: Shape[] = [];
    let totalArea = 0;
    for (let i = 0; i < counts.length && i < allShapes.length; i++) {
        for (let q = 0; q < counts[i]; q++) {
            toFit.push(allShapes[i]);
            totalArea += allShapes[i].area;
        }
    }
    toFit.sort((a, b) => b.area - a.area);
    if (solve(0, new Array(W * H).fill(false), W, H, toFit, totalArea, W * H)) inc(1);
}

function main() {
    const data = readFileSync('input.txt', 'utf8');
    const lines = data.split(/\r?\n/);
    const allShapes: Shape[] = [];
    let totalPossible = 0;
    for (let i = 0; i < lines.length; ) {
        const line = lines[i].trim();
        if (!line) { i++; continue; }
        if (line.includes('x') && line.includes(':')) {
            handleRegion(line, allShapes, v => totalPossible += v);
            i++;
        } else if (line.endsWith(':')) {
            const rows: string[] = [];
            i++;
            while (i < lines.length) {
                const nxt = lines[i].trim();
                if (!nxt || nxt.includes(':')) break;
                if (nxt.includes('#') || nxt.includes('.')) rows.push(nxt);
                else break;
                i++;
            }
            if (rows.length) allShapes.push(new Shape(rows));
        } else i++;
    }
    console.log(totalPossible);
}
main();
