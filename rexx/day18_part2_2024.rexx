
/* REXX */
main:
  size = 71
  inputFile = 'input.txt'
  grid. = 0

  do while lines(inputFile) > 0
    line = linein(inputFile)
    parse var line x ',' y

    if x >= 0 & x < size & y >= 0 & y < size then
      grid.y.x = 1

    if \canReach(size) then do
      say x','y
      exit
    end
  end

  say 'No cutoff found'
  exit

canReach:
  procedure expose grid.
  parse arg n
  nm1 = n - 1

  if grid.0.0 = 1 | grid.nm1.nm1 = 1 then return 0

  visited. = 0
  visited.0.0 = 1

  q. = ''
  q.1 = '0 0'
  head = 1
  tail = 2

  do while head < tail
    parse var q.head cx cy
    head = head + 1

    if cx = nm1 & cy = nm1 then return 1

    call processNeighbor cx + 1, cy, n
    call processNeighbor cx - 1, cy, n
    call processNeighbor cx, cy + 1, n
    call processNeighbor cx, cy - 1, n
  end

  return 0

processNeighbor:
  procedure expose grid. visited. q. head tail
  parse arg nx, ny, n
  if nx >= 0 & ny >= 0 & nx < n & ny < n then
    if grid.ny.nx = 0 & visited.ny.nx = 0 then do
      visited.ny.nx = 1
      q.tail = nx ny
      tail = tail + 1
    end
  return
