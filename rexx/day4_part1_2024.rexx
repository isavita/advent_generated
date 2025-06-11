
/* Rexx */
call main
exit

main:
  word = 'XMAS'
  wordLen = length(word)
  count = 0
  inputFile = 'input.txt'

  dx.1 =  0; dy.1 =  1  /* Right */
  dx.2 =  1; dy.2 =  0  /* Down */
  dx.3 =  1; dy.3 =  1  /* Down-Right */
  dx.4 = -1; dy.4 =  1  /* Up-Right */
  dx.5 =  0; dy.5 = -1  /* Left */
  dx.6 = -1; dy.6 =  0  /* Up */
  dx.7 = -1; dy.7 = -1  /* Up-Left */
  dx.8 =  1; dy.8 = -1  /* Down-Left */

  rows = 0
  do while lines(inputFile) > 0
    rows = rows + 1
    grid.rows = linein(inputFile)
  end
  call stream inputFile, 'C', 'CLOSE'

  if rows > 0 then
    cols = length(grid.1)
  else
    cols = 0

  do r = 1 to rows
    do c = 1 to cols
      do d = 1 to 8
        if checkWord(r, c, d) then
          count = count + 1
      end
    end
  end

  say 'XMAS appears' count 'times in the word search'
return

checkWord:
  procedure expose grid. rows cols word wordLen dx. dy.
  parse arg startR, startC, direction

  do i = 1 to wordLen
    currR = startR + (dx.direction * (i - 1))
    currC = startC + (dy.direction * (i - 1))

    if currR < 1 | currR > rows then return 0
    if currC < 1 | currC > cols then return 0

    if substr(grid.currR, currC, 1) \= substr(word, i, 1) then
      return 0
  end

return 1
