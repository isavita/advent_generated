
import strutils, sequtils, bitops

type Bitset = array[32, uint64]

proc setBit(b: var Bitset, i: int) = b[i shr 6] = b[i shr 6] or (1'u64 shl (i and 63))
proc testBit(b: Bitset, i: int): bool = (b[i shr 6] and (1'u64 shl (i and 63))) != 0
proc `^=`(a: var Bitset, b: Bitset) =
  for i in 0..<32: a[i] = a[i] xor b[i]

proc solve() =
  let content = try: readFile("input.txt") except: ""
  var total: int64 = 0
  for line in content.splitLines():
    let sIdx = line.find('[')
    let eIdx = line.find(']')
    if sIdx == -1: continue
    let targetStr = line[sIdx+1..<eIdx]
    let R = targetStr.len
    var buttons = newSeq[seq[int]]()
    var cur = eIdx + 1
    while true:
      let bStart = line.find('(', cur)
      if bStart == -1: break
      let bEnd = line.find(')', bStart)
      if bEnd == -1: break
      let part = line[bStart+1..<bEnd].strip()
      buttons.add(if part == "": @[] else: part.split(',').mapIt(it.strip.parseInt))
      cur = bEnd + 1
    let C = buttons.len
    var mat = newSeq[Bitset](R)
    for i, c in targetStr:
      if c == '#': mat[i].setBit(C)
    for j in 0..<C:
      for lamp in buttons[j]:
        if lamp < R: mat[lamp].setBit(j)
    var pr = 0
    var isP = newSeq[bool](C)
    for c in 0..<C:
      if pr >= R: break
      var sel = -1
      for r in pr..<R:
        if mat[r].testBit(c): (sel = r; break)
      if sel == -1: continue
      swap(mat[pr], mat[sel])
      for r in 0..<R:
        if r != pr and mat[r].testBit(c): mat[r] ^= mat[pr]
      isP[c] = true; pr += 1
    var ok = true
    for r in pr..<R:
      if mat[r].testBit(C): ok = false; break
    if not ok: continue
    var fv = newSeq[int]()
    for c in 0..<C:
      if not isP[c]: fv.add(c)
    let nf = fv.len
    if nf > 62: continue
    var fvMasks = newSeq[uint64](pr)
    for r in 0..<pr:
      for j in 0..<nf:
        if mat[r].testBit(fv[j]): fvMasks[r] = fvMasks[r] or (1'u64 shl j)
    var minW = 1000000000
    for i in 0'u64 ..< (1'u64 shl nf):
      var cw = countSetBits(i)
      for r in 0..<pr:
        let parity = countSetBits(fvMasks[r] and i) and 1
        if (ord(mat[r].testBit(C)) xor parity) == 1: cw += 1
      if cw < minW: minW = cw
    if minW != 1000000000: total += minW
  echo "Fewest total button presses: ", total

solve()

