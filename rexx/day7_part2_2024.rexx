
/* Rexx */
main:
  numeric digits 64
  total_sum = 0
  filename = 'input.txt'

  do while lines(filename) > 0
    line = linein(filename)
    parse var line test_value ':' numbers_str
    test_value = strip(test_value)
    numbers.0 = 0
    do i = 1 to words(numbers_str)
      numbers.i = word(numbers_str, i)
      numbers.0 = i
    end
    if numbers.0 > 0 then
      if evaluate(test_value, 2, numbers.1) then
        total_sum = total_sum + test_value
  end
  close(filename)

  say total_sum
exit

evaluate: procedure expose numbers.
  arg target, index, current_result

  if index > numbers.0 then
    return (target = current_result)

  next_number = numbers.index

  if evaluate(target, index + 1, current_result + next_number) then return 1
  if evaluate(target, index + 1, current_result * next_number) then return 1
  if evaluate(target, index + 1, current_result || next_number) then return 1

  return 0
