
/* Rexx */
main:
  parse value 0 0 with h w
  call stream 'input.txt', 'c', 'open read'
  do h = 1 while lines('input.txt') > 0
    grid.h = linein('input.txt')
  end
  call stream 'input.txt', 'c', 'close'
  w = length(grid.1)

  dirX.0 = 0;  dirY.0 = -1 /* Up    */
  dirX.1 = 1;  dirY.1 = 0  /* Right */
  dirX.2 = 0;  dirY.2 = 1  /* Down  */
  dirX.3 = -1; dirY.3 = 0  /* Left  */

  found = 0
  do y = 1 to h while \found
    line = grid.y
    if pos('^', line) > 0 then do; x = pos('^', line); dirIdx = 0; found=1; end
    if pos('>', line) > 0 then do; x = pos('>', line); dirIdx = 1; found=1; end
    if pos('v', line) > 0 then do; x = pos('v', line); dirIdx = 2; found=1; end
    if pos('<', line) > 0 then do; x = pos('<', line); dirIdx = 3; found=1; end
  end

  visited. = 0
  visited.y.x = 1
  count = 1

  do forever
    nx = x + dirX.dirIdx
    ny = y + dirY.dirIdx

    if nx < 1 | nx > w | ny < 1 | ny > h then leave

    if substr(grid.ny, nx, 1) = '#' then do
      dirIdx = (dirIdx + 1) // 4
      iterate
    end

    x = nx
    y = ny

    if visited.y.x = 0 then do
      visited.y.x = 1
      count = count + 1
    end
  end

  say count
exit
