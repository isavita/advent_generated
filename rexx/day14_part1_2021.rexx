
/* REXX */
call main
exit

main:
  rules. = ''
  fileName = 'input.txt'
  polymer = linein(fileName)
  call linein(fileName)

  do while lines(fileName) > 0
    line = linein(fileName)
    if line = '' then iterate
    parse var line pair ' -> ' insert
    rules.pair = insert
  end

  do step = 1 to 10
    newPolymer = ''
    do i = 1 to length(polymer) - 1
      pair = substr(polymer, i, 2)
      newPolymer = newPolymer || substr(pair, 1, 1) || rules.pair
    end
    polymer = newPolymer || substr(polymer, length(polymer), 1)
  end

  counts. = 0
  uniqueChars = ''
  do i = 1 to length(polymer)
    char = substr(polymer, i, 1)
    counts.char = counts.char + 1
    if pos(char, uniqueChars) = 0 then
      uniqueChars = uniqueChars || char
  end

  maxCount = 0
  minCount = length(polymer)
  do i = 1 to length(uniqueChars)
    char = substr(uniqueChars, i, 1)
    count = counts.char
    maxCount = max(maxCount, count)
    minCount = min(minCount, count)
  end

  say maxCount - minCount
return
