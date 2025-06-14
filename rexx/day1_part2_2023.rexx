
main:
  sum = 0
  digits.0 = 'zero'; digits.1 = 'one';   digits.2 = 'two'
  digits.3 = 'three'; digits.4 = 'four';  digits.5 = 'five'
  digits.6 = 'six';   digits.7 = 'seven'; digits.8 = 'eight'
  digits.9 = 'nine'
  fileName = 'input.txt'

  call stream fileName, 'c', 'open read'
  do while lines(fileName) > 0
    line = linein(fileName)
    sum = sum + get_line_value(line)
  end
  call stream fileName, 'c', 'close'

  say sum
exit

get_line_value: procedure expose digits.
  parse arg line
  first = ''
  last = ''
  do i = 1 to length(line)
    char = substr(line, i, 1)
    val = -1
    if datatype(char, 'W') then
      val = char
    else do j = 0 to 9
      if substr(line, i, length(digits.j)) = digits.j then do
        val = j
        leave
      end
    end

    if val <> -1 then do
      if first = '' then first = val
      last = val
    end
  end
  return first || last
