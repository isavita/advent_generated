
/* REXX */
call main
exit

main:
  checksum = 0
  fileName = 'input.txt'
  do while lines(fileName) > 0
    line = linein(fileName)
    if words(line) = 0 then iterate

    min_val = word(line, 1)
    max_val = word(line, 1)

    do i = 2 to words(line)
      num = word(line, i)
      min_val = min(min_val, num)
      max_val = max(max_val, num)
    end
    checksum = checksum + (max_val - min_val)
  end
  say checksum
return
