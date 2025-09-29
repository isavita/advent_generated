import os

proc toInt(s: string): int =
  var i = 0
  var sign = 1
  var v = 0
  if s.len > 0 and s[0] == '-':
    sign = -1
    i = 1
  while i < s.len:
    let c = s[i]
    if c >= '0' and c <= '9':
      v = v * 10 + (ord(c) - ord('0'))
    else:
      break
    inc(i)
  return v * sign

proc main() =
  let input = readFile("input.txt")
  var ground: seq[string] = @["+"]  # initial grid
  var maxX = 0
  var minX = 0
  var maxY = 0
  var minY = 20
  const xOffset = 500
  const yOffset = 0

  # split input into lines
  var lines: seq[string] = @[]
  var cur = ""
  for ch in input:
    if ch == '\n':
      lines.add(cur)
      cur = ""
    else:
      cur.add ch
  if cur.len > 0:
    lines.add(cur)

  for line in lines:
    if line.len == 0: continue
    # extract all integers from line
    var nums: seq[int] = @[]
    var curNum = ""
    for ch in line:
      if ch >= '0' and ch <= '9':
        curNum.add ch
      else:
        if curNum.len > 0:
          nums.add(toInt(curNum))
          curNum = ""
    if curNum.len > 0:
      nums.add(toInt(curNum))

    if line.len > 0 and line[0] == 'x':
      if nums.len < 3: continue
      let xVal = nums[0] - xOffset
      let y1 = nums[1] - yOffset
      let y2 = nums[2] - yOffset

      while xVal >= maxX:
        maxX += 1
        for i in 0 ..< ground.len:
          ground[i] = ground[i] & "."
      while xVal <= minX:
        minX -= 1
        for i in 0 ..< ground.len:
          ground[i] = "." & ground[i]
      while y2 > maxY:
        maxY += 1
        var newRow = ""
        if ground.len > 0:
          let w = ground[0].len
          for _ in 0 ..< w:
            newRow.add '.'
        ground.add(newRow)
      if y1 < minY: minY = y1
      var y = y1
      while y <= y2:
        var row = ground[y]
        let idx = xVal - minX
        row[idx] = '#'
        ground[y] = row
        inc(y)
    else:
      if nums.len >= 3:
        let yVal = nums[0] - yOffset
        let x1 = nums[1] - xOffset
        let x2 = nums[2] - xOffset

        while yVal > maxY:
          maxY += 1
          var newRow = ""
          if ground.len > 0:
            let w = ground[0].len
            for _ in 0 ..< w:
              newRow.add '.'
          ground.add(newRow)
        while x2 >= maxX:
          maxX += 1
          for i in 0 ..< ground.len:
            ground[i] = ground[i] & "."
        while x1 <= minX:
          minX -= 1
          for i in 0 ..< ground.len:
            ground[i] = "." & ground[i]
        let y = yVal
        for xi in x1 .. x2:
          var row = ground[y]
          row[xi - minX] = '#'
          ground[y] = row
        if yVal < minY: minY = yVal

  var water_count = 0
  var flow_count = 0
  const round_limit = 200000

  let startX = -minX
  while ground[1][startX] != '|' and water_count < round_limit:
    var can_move = true
    var x = startX
    var y = 1
    var try_left = 0
    while can_move:
      if y + 1 > maxY or ground[y + 1][x] == '|':
        var row = ground[y]
        row[x] = '|'
        ground[y] = row
        can_move = false
        if y >= minY:
          inc(flow_count)
      elif ground[y + 1][x] == '.':
        y += 1
        try_left = 0
      elif ground[y + 1][x] == '#' or ground[y + 1][x] == '~':
        let leftChar = ground[y][x - 1]
        let rightChar = ground[y][x + 1]
        if (try_left == 1 and leftChar == '|') or
           (try_left == 2 and rightChar == '|') or
           (rightChar == '|' and leftChar != '.') or
           (rightChar != '.' and leftChar == '|'):
          var row = ground[y]
          row[x] = '|'
          ground[y] = row
          inc(flow_count)
          can_move = false
          var i = x + 1
          while i < ground[0].len:
            if ground[y][i] == '~':
              var row2 = ground[y]
              row2[i] = '|'
              ground[y] = row2
              dec water_count
              inc flow_count
              inc(i)
            else:
              break
          var j = x - 1
          while j >= 0:
            if ground[y][j] == '~':
              var row2 = ground[y]
              row2[j] = '|'
              ground[y] = row2
              dec water_count
              inc flow_count
              dec j
            else:
              break
        elif ( (try_left == 0 and ground[y][x - 1] == '.') or (try_left == 1 and ground[y][x - 1] == '.') ):
          x -= 1
          try_left = 1
        elif ( (try_left == 0 and ground[y][x + 1] == '.') or (try_left == 2 and ground[y][x + 1] == '.') ):
          x += 1
          try_left = 2
        else:
          can_move = false
          var row = ground[y]
          row[x] = '~'
          ground[y] = row
          inc(water_count)
      else:
        break

  echo(flow_count + water_count)

when isMainModule:
  main()