
fs = require 'fs'

main = ->
  data = fs.readFileSync 'input.txt', 'utf8'
  lines = data.trim().split /\r?\n/
  pts = []
  xs = []
  ys = []
  for line in lines
    line = line.replace /,/g, ' '
    [x, y] = line.trim().split(/\s+/).map Number
    continue unless isFinite x and isFinite y
    pts.push {x, y}
    xs.push x
    ys.push y
  if pts.length is 0
    console.log "Largest valid area: 0"
    return

  xs = xs.sort((a,b)->a-b).filter (v,i,a)->i is 0 or v isnt a[i-1]
  ys = ys.sort((a,b)->a-b).filter (v,i,a)->i is 0 or v isnt a[i-1]
  ux = xs.length
  uy = ys.length
  xidx = {}
  yidx = {}
  for v,i in xs then xidx[v] = i
  for v,i in ys then yidx[v] = i

  W = 2*ux + 1
  H = 2*uy + 1
  colW = new Array(W).fill 0
  rowH = new Array(H).fill 0
  colW[0] = 1
  for i in [0...ux]
    colW[2*i+1] = 1
    if i+1 < ux
      gap = xs[i+1] - xs[i] - 1
      colW[2*i+2] = if gap>0 then gap else 0
    else
      colW[2*i+2] = 1
  rowH[0] = 1
  for i in [0...uy]
    rowH[2*i+1] = 1
    if i+1 < uy
      gap = ys[i+1] - ys[i] - 1
      rowH[2*i+2] = if gap>0 then gap else 0
    else
      rowH[2*i+2] = 1

  grid = (new Array(H)).fill(0).map -> new Array(W).fill 0
  n = pts.length
  for i in [0...n]
    a = pts[i]
    b = pts[(i+1)%n]
    gx1 = 2*xidx[a.x] + 1
    gy1 = 2*yidx[a.y] + 1
    gx2 = 2*xidx[b.x] + 1
    gy2 = 2*yidx[b.y] + 1
    if gx1 is gx2
      y0 = Math.min gy1, gy2
      y1 = Math.max gy1, gy2
      for y in [y0..y1] when rowH[y] > 0
        grid[y][gx1] = 1
    else
      x0 = Math.min gx1, gx2
      x1 = Math.max gx1, gx2
      for x in [x0..x1] when colW[x] > 0
        grid[gy1][x] = 1

  q = []
  qi = 0
  grid[0][0] = 2
  q.push [0,0]
  dirs = [[0,1],[0,-1],[1,0],[-1,0]]
  while qi < q.length
    [cx,cy] = q[qi++]
    for [dx,dy] in dirs
      nx = cx+dx
      ny = cy+dy
      if 0<=nx< W and 0<=ny< H and grid[ny][nx] is 0
        grid[ny][nx] = 2
        q.push [nx,ny]

  P = (new Array(H)).fill(0).map -> new Array(W).fill 0
  for y in [0...H]
    rowSum = 0
    for x in [0...W]
      val = if grid[y][x] isnt 2 then colW[x]*rowH[y] else 0
      rowSum += val
      above = if y>0 then P[y-1][x] else 0
      P[y][x] = rowSum + above

  maxArea = 0
  for i in [0...n]
    for j in [i...n]
      w = Math.abs(pts[i].x-pts[j].x)+1
      h = Math.abs(pts[i].y-pts[j].y)+1
      area = w*h
      continue if area <= maxArea
      gx1 = 2*xidx[pts[i].x] + 1
      gy1 = 2*yidx[pts[i].y] + 1
      gx2 = 2*xidx[pts[j].x] + 1
      gy2 = 2*yidx[pts[j].y] + 1
      if gx1 > gx2 then [gx1,gx2] = [gx2,gx1]
      if gy1 > gy2 then [gy1,gy2] = [gy2,gy1]
      total = P[gy2][gx2]
      left = if gx1 then P[gy2][gx1-1] else 0
      up = if gy1 then P[gy1-1][gx2] else 0
      diag = if gx1 and gy1 then P[gy1-1][gx1-1] else 0
      valid = total - left - up + diag
      maxArea = area if valid is area

  console.log "Largest valid area: #{maxArea}"

main()
