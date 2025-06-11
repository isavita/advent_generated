
/* Rexx */
call main
exit

main:
  inputFile = 'input.txt'
  totalCount = 0
  isFirstInGroup = 1
  groupAnswers = ''

  do while lines(inputFile) > 0
    line = strip(linein(inputFile))

    if line = '' then do
      totalCount = totalCount + length(groupAnswers)
      isFirstInGroup = 1
      groupAnswers = ''
      iterate
    end

    if isFirstInGroup then do
      groupAnswers = line
      isFirstInGroup = 0
    end
    else do
      newAnswers = ''
      do i = 1 to length(groupAnswers)
        char = substr(groupAnswers, i, 1)
        if pos(char, line) > 0 then
          newAnswers = newAnswers || char
      end
      groupAnswers = newAnswers
    end
  end

  totalCount = totalCount + length(groupAnswers)
  say totalCount
return
