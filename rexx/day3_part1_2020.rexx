
/* Rexx */
call main

main:
  inputFile = 'input.txt'
  i = 0
  do i = 1 while lines(inputFile) > 0
    forest.i = linein(inputFile)
  end
  forest.0 = i - 1

  right = 3
  down = 1
  trees = 0
  x = 1
  width = length(forest.1)

  do y = 1 to forest.0 by down
    pos = (x - 1) // width + 1
    if substr(forest.y, pos, 1) = '#' then
      trees = trees + 1
    x = x + right
  end

  say trees
  return
