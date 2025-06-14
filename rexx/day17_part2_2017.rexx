
/* Rexx solution */
main:
  steps = linein('input.txt')
  currentPos = 0
  valueAfterZero = 0

  do i = 1 to 50000000
    currentPos = (currentPos + steps) // i
    if currentPos = 0 then
      valueAfterZero = i
    currentPos = currentPos + 1
  end

  say valueAfterZero
exit
