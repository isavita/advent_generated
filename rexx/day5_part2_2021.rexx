
/* Rexx */
call main
exit

main:
  inputFile = 'input.txt'
  overlaps. = 0
  count = 0

  do while lines(inputFile) > 0
    line = linein(inputFile)
    parse var line x1 ',' y1 ' -> ' x2 ',' y2

    xStep = sign(x2 - x1)
    yStep = sign(y2 - y1)
    steps = max(abs(x2 - x1), abs(y2 - y1))

    x = x1
    y = y1
    do i = 0 to steps
      key = x'.'y
      overlaps.key = overlaps.key + 1
      if overlaps.key = 2 then
        count = count + 1

      x = x + xStep
      y = y + yStep
    end
  end

  call stream inputFile, 'c', 'close'
  say count
return
