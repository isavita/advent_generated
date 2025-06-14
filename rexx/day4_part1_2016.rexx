
/* Rexx */
call main
exit

main:
  totalSectorID = 0
  fileName = 'input.txt'
  call stream fileName, 'c', 'open read'

  do while lines(fileName) > 0
    line = linein(fileName)
    last_dash = lastpos('-', line)
    name = substr(line, 1, last_dash - 1)
    rest = substr(line, last_dash + 1)
    parse var rest sectorID '[' checksum ']'

    if isRealRoom(name, checksum) then
      totalSectorID = totalSectorID + sectorID
  end

  call stream fileName, 'c', 'close'
  say totalSectorID
return

isRealRoom: procedure
  parse arg name, expectedChecksum
  name = translate(name, '', '-')
  letters. = 0
  alphabet = 'abcdefghijklmnopqrstuvwxyz'

  do i = 1 to length(name)
    char = substr(name, i, 1)
    letters.char = letters.char + 1
  end

  calculatedChecksum = ''
  do i = 1 to 5
    maxCount = -1
    maxChar = ''
    do j = 1 to length(alphabet)
      char = substr(alphabet, j, 1)
      if letters.char > maxCount then do
        maxCount = letters.char
        maxChar = char
      end
    end
    if maxChar <> '' then do
      calculatedChecksum = calculatedChecksum || maxChar
      letters.maxChar = -1
    end
  end

  return (calculatedChecksum = expectedChecksum)
