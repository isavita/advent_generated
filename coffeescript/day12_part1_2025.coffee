
fs = require 'fs'

class Shape
  constructor: (rows) ->
    pts = []
    rows.forEach (row, r) ->
      row.split('').forEach (ch, c) ->
        pts.push [c, r] if ch is '#'
    @area = pts.length
    @orientations = @genOrients pts

  genOrients: (pts) ->
    uniq = {}
    for t in [0...8]
      trans = pts.map (p) ->
        x = p[0]; y = p[1]
        switch t
          when 0 then [ x,  y]
          when 1 then [ y, -x]
          when 2 then [-x, -y]
          when 3 then [-y,  x]
          when 4 then [-x,  y]
          when 5 then [ y,  x]
          when 6 then [ x, -y]
          when 7 then [-y, -x]
      minX = Math.min.apply null, trans.map (p) -> p[0]
      minY = Math.min.apply null, trans.map (p) -> p[1]
      trans = trans.map (p) -> [p[0]-minX, p[1]-minY]
      trans.sort (a,b) -> a[1]-b[1] or a[0]-b[0]
      key = trans.map((p)->p.join(',')).join(';')
      unless uniq[key]
        uniq[key] = trans
    (Object.values uniq).map (pts) ->
      w = Math.max.apply null, pts.map (p) -> p[0] + 1
      h = Math.max.apply null, pts.map (p) -> p[1] + 1
      {pts, w, h}

solve = (idx, grid, W, H, shapes, remArea, freeArea) ->
  return true if idx is shapes.length
  return false if remArea > freeArea
  shape = shapes[idx]
  for o in shape.orientations
    continue if o.w > W or o.h > H
    for r in [0..H-o.h]
      for c in [0..W-o.w]
        ok = true
        for p in o.pts
          pos = (r+p[1])*W + (c+p[0])
          if grid[pos] then ok = false; break
        continue unless ok
        for p in o.pts
          grid[(r+p[1])*W + (c+p[0])] = true
        if solve(idx+1, grid, W, H, shapes, remArea-shape.area, freeArea-shape.area) then return true
        for p in o.pts
          grid[(r+p[1])*W + (c+p[0])] = false
  false

main = ->
  data = fs.readFileSync 'input.txt', 'utf8'
  lines = data.split /\r?\n/
  i = 0
  shapes = []
  total = 0
  while i < lines.length
    line = lines[i].trim()
    i++
    continue if line is ''
    if line.includes('x') and line.includes(':')
      [dim, cnts] = line.split ':'
      [W, H] = dim.split('x').map Number
      need = cnts.trim().split(/\s+/).map Number
      toFit = []
      area = 0
      need.forEach (q, idx) ->
        for _ in [0...q]
          s = shapes[idx]
          toFit.push s
          area += s.area
      toFit.sort (a,b) -> b.area - a.area
      grid = new Array(W*H).fill false
      total++ if solve 0, grid, W, H, toFit, area, W*H
    else if line.endsWith ':'
      rows = []
      while i < lines.length
        nxt = lines[i].trim()
        break if nxt is '' or nxt.includes(':')
        rows.push nxt if nxt.includes('#') or nxt.includes('.')
        i++
      shapes.push new Shape rows if rows.length
  console.log total

main()
