
/* REXX */
main:
  count = 0
  fileName = 'input.txt'
  validLengths = ' 2 3 4 7 '

  call lineout fileName /* Close file if open from previous runs */

  do while lines(fileName) > 0
    line = linein(fileName)
    output = substr(line, pos(' | ', line) + 3)
    do i = 1 to words(output)
      len = length(word(output, i))
      if pos(' ' || len || ' ', validLengths) > 0 then
        count = count + 1
    end
  end

  say count
  exit
