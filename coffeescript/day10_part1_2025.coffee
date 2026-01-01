
fs = require 'fs'

main = ->
  try
    data = fs.readFileSync('input.txt', 'utf8').split '\n'
  catch
    return
    
  ans = 0
  for line in data
    continue unless m = line.match /\[(.*?)\]/
    target = m[1]
    R = target.length
    btns = []
    reg = /\(([^)]*)\)/g
    while (b = reg.exec line)
      btns.push if b[1].trim() then b[1].split(',').map(Number) else []
    
    C = btns.length
    mat = Array.from {length: R}, -> new Uint8Array C + 1
    for c in [0...C]
      for r in btns[c] when r < R
        mat[r][c] = 1
    for r in [0...R] when target[r] is '#'
      mat[r][C] = 1

    pRow = 0
    pMap = new Array(C).fill -1
    for c in [0...C] when pRow < R
      sel = -1
      for r in [pRow...R] when mat[r][c]
        sel = r
        break
      continue if sel is -1
      
      [mat[pRow], mat[sel]] = [mat[sel], mat[pRow]]
      for r in [0...R] when r isnt pRow and mat[r][c]
        for k in [c..C]
          mat[r][k] ^= mat[pRow][k]
      pMap[c] = pRow++

    ok = true
    for r in [pRow...R] when mat[r][C]
      ok = false
    continue unless ok

    free = (c for c in [0...C] when pMap[c] is -1)
    best = Infinity
    
    for mask in [0...Math.pow(2, free.length)]
      x = new Uint8Array C
      cw = 0
      for bit, j in free when (mask / Math.pow(2, j)) & 1
        x[bit] = 1
        cw++
      
      for c in [0...C] when (pr = pMap[c]) isnt -1
        v = mat[pr][C]
        for k in [c + 1...C] when mat[pr][k]
          v ^= x[k]
        x[c] = v
        cw++ if v
      
      best = if cw < best then cw else best
    
    ans += best if best isnt Infinity

  console.log ans

main()
