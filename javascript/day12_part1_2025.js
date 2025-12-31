
const fs = require('fs')
const lines = fs.readFileSync('input.txt', 'utf8').split(/\r?\n/)

function trim(s) { return s.trim() }
function cmp(a, b) { return a.r !== b.r ? a.r - b.r : a.c - b.c }

function normalize(piece) {
  if (!piece.p.length) return piece
  const minR = Math.min(...piece.p.map(pt => pt.r))
  const minC = Math.min(...piece.p.map(pt => pt.c))
  const arr = piece.p.map(pt => ({ r: pt.r - minR, c: pt.c - minC })).sort(cmp)
  return { p: arr, n: arr.length }
}
function rotate(piece) {
  const arr = piece.p.map(pt => ({ r: pt.c, c: -pt.r }))
  return { p: arr, n: piece.n }
}
function flip(piece) {
  const arr = piece.p.map(pt => ({ r: pt.r, c: -pt.c }))
  return { p: arr, n: piece.n }
}
function equal(a, b) {
  if (a.n !== b.n) return false
  for (let i = 0; i < a.n; i++) {
    if (a.p[i].r !== b.p[i].r || a.p[i].c !== b.p[i].c) return false
  }
  return true
}
function generateVariations(base) {
  const uniq = []
  let cur = base
  for (let k = 0; k < 4; k++) {
    const n = normalize(cur)
    if (!uniq.some(v => equal(v, n))) uniq.push(n)
    const f = flip(cur)
    const nf = normalize(f)
    if (!uniq.some(v => equal(v, nf))) uniq.push(nf)
    cur = rotate(cur)
  }
  return uniq
}
function canPlace(rows, cols, grid, p, r, c) {
  for (let i = 0; i < p.n; i++) {
    const nr = r + p.p[i].r, nc = c + p.p[i].c
    if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) return false
    if (grid[nr * cols + nc]) return false
  }
  return true
}
function place(cols, grid, p, r, c, v) {
  for (let i = 0; i < p.n; i++) grid[(r + p.p[i].r) * cols + (c + p.p[i].c)] = v
}
function checkIslands(rows, cols, grid, counts, arrSize, slackIdx, shapes) {
  let minReal = Number.MAX_SAFE_INTEGER, hasReal = false
  for (let i = 0; i < arrSize; i++) if (i !== slackIdx && counts[i] > 0) {
    if (shapes[i].n < minReal) minReal = shapes[i].n
    hasReal = true
  }
  if (!hasReal) return true
  let avail = counts[slackIdx]
  const vis = new Uint8Array(rows * cols)
  const q = new Int32Array(rows * cols)
  for (let idx = 0; idx < rows * cols; idx++) {
    if (grid[idx] || vis[idx]) continue
    let qs = 0, qe = 0, size = 0
    q[qe++] = idx; vis[idx] = 1
    while (qs < qe) {
      const cur = q[qs++]; size++
      const r = (cur / cols) | 0, c = cur % cols
      if (r > 0) { const n = cur - cols; if (!grid[n] && !vis[n]) {vis[n]=1; q[qe++]=n} }
      if (r < rows - 1) { const n = cur + cols; if (!grid[n] && !vis[n]) {vis[n]=1; q[qe++]=n} }
      if (c > 0) { const n = cur - 1; if (!grid[n] && !vis[n]) {vis[n]=1; q[qe++]=n} }
      if (c < cols - 1) { const n = cur + 1; if (!grid[n] && !vis[n]) {vis[n]=1; q[qe++]=n} }
    }
    if (size < minReal) {
      if (avail >= size) avail -= size
      else return false
    }
  }
  return true
}
function solveRec(rows, cols, grid, counts, arrSize, ids, varCounts, variations, slackIdx, shapes) {
  let empty = -1
  for (let i = 0; i < rows * cols; i++) if (!grid[i]) { empty = i; break }
  if (empty === -1) return true
  const r = (empty / cols) | 0, c = empty % cols
  if (!checkIslands(rows, cols, grid, counts, arrSize, slackIdx, shapes)) return false
  for (let ii = 0; ii < ids.length; ii++) {
    const id = ids[ii]
    if (!counts[id]) continue
    counts[id]--
    const vars = variations[id]
    for (let v = 0; v < varCounts[id]; v++) {
      const p = vars[v]
      if (canPlace(rows, cols, grid, p, r, c)) {
        place(cols, grid, p, r, c, 1)
        if (solveRec(rows, cols, grid, counts, arrSize, ids, varCounts, variations, slackIdx, shapes)) return true
        place(cols, grid, p, r, c, 0)
      }
    }
    counts[id]++
  }
  return false
}

// parsing
let maxId = -1
lines.forEach(l => {
  const s = trim(l)
  if (s && s.endsWith(':')) {
    const id = +s.slice(0, -1)
    if (id > maxId) maxId = id
  }
})
if (maxId < 0) maxId = -1
const arrSize = maxId + 2
const slackIdx = maxId + 1
const shapes = Array.from({length: arrSize}, () => ({p: [], n: 0}))
let parsingShapes = true, curId = -1, curShape = []
const regionLines = []
lines.forEach(raw => {
  const s = trim(raw)
  if (!s) return
  if (s.includes('x') && s.includes(':')) parsingShapes = false
  if (parsingShapes) {
    if (s.endsWith(':')) {
      if (curId !== -1 && curShape.length) {
        const pts = []
        curShape.forEach((row, rr) => {
          for (let cc = 0; cc < row.length; cc++) if (row[cc] === '#') pts.push({r: rr, c: cc})
        })
        shapes[curId] = normalize({p: pts, n: pts.length})
      }
      curId = +s.slice(0, -1)
      curShape = []
    } else curShape.push(s)
  } else regionLines.push(s)
})
if (curId !== -1 && curShape.length) {
  const pts = []
  curShape.forEach((row, rr) => {
    for (let cc = 0; cc < row.length; cc++) if (row[cc] === '#') pts.push({r: rr, c: cc})
  })
  shapes[curId] = normalize({p: pts, n: pts.length})
}
shapes[slackIdx] = {p: [{r:0,c:0}], n:1}

const variations = Array.from({length: arrSize}, () => [])
const varCounts = new Int32Array(arrSize)
for (let i = 0; i < arrSize; i++) {
  if (shapes[i].n) {
    const vars = generateVariations(shapes[i])
    variations[i] = vars
    varCounts[i] = vars.length
  }
}

let solved = 0
regionLines.forEach(line => {
  const parts = line.split(':')
  if (parts.length !== 2) return
  const dims = parts[0].trim()
  const countsStr = parts[1].trim()
  const [wxStr, hStr] = dims.split('x')
  const wx = +wxStr, h = +hStr
  const gridSize = wx * h
  const pieceCounts = new Int32Array(arrSize)
  let totalArea = 0
  const toks = countsStr.split(/\s+/)
  for (let i = 0; i < toks.length && i < arrSize - 1; i++) {
    const c = +toks[i]
    if (c) {
      pieceCounts[i] = c
      totalArea += c * shapes[i].n
    }
  }
  if (totalArea > gridSize) return
  const slack = gridSize - totalArea
  if (slack) pieceCounts[slackIdx] = slack
  const ids = []
  for (let i = 0; i < arrSize; i++) if (pieceCounts[i]) ids.push(i)
  ids.sort((a, b) => shapes[b].n - shapes[a].n)
  const grid = new Uint8Array(gridSize)
  if (solveRec(h, wx, grid, pieceCounts, arrSize, ids, varCounts, variations, slackIdx, shapes)) solved++
})

console.log(`Number of regions that fit all presents: ${solved}`)
