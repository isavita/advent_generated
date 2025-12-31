
import { readFileSync } from 'fs'

function main() {
    const data = readFileSync('input.txt', 'utf8')
    const adj = new Map<string, string[]>()
    for (const line of data.split('\n')) {
        const l = line.trim()
        if (!l || !l.includes(':')) continue
        const [node, rest] = l.split(':', 2)
        const targets = rest.trim().split(/\s+/).filter(Boolean)
        const cur = adj.get(node) ?? []
        cur.push(...targets)
        adj.set(node, cur)
    }

    const memo = new Map<string, bigint>()
    const count = (src: string, dst: string): bigint => {
        if (src === dst) return 1n
        const key = src + '->' + dst
        if (memo.has(key)) return memo.get(key)!
        let total = 0n
        const nxt = adj.get(src)
        if (nxt) {
            for (const v of nxt) total += count(v, dst)
        }
        memo.set(key, total)
        return total
    }

    const p1 = count('svr', 'dac') * count('dac', 'fft') * count('fft', 'out')
    const p2 = count('svr', 'fft') * count('fft', 'dac') * count('dac', 'out')
    console.log((p1 + p2).toString())
}

main()
