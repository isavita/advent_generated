
call main
exit

main:
  inputFile = 'input.txt'
  lines.0 = 0
  do i = 1 while lines(inputFile) > 0
    lines.i = linein(inputFile)
    lines.0 = i
  end
  call stream inputFile, 'C', 'CLOSE'

  if lines.0 = 0 then return

  message_len = length(lines.1)
  message1 = ''
  message2 = ''

  do i = 1 to message_len
    counts. = 0
    chars = ''
    do j = 1 to lines.0
      char = substr(lines.j, i, 1)
      if counts.char = 0 then
        chars = chars || char
      counts.char = counts.char + 1
    end

    max_count = 0
    min_count = lines.0 + 1
    most_common = ''
    least_common = ''

    do k = 1 to length(chars)
      char = substr(chars, k, 1)
      count = counts.char
      if count > max_count then do
        max_count = count
        most_common = char
      end
      if count < min_count then do
        min_count = count
        least_common = char
      end
    end

    message1 = message1 || most_common
    message2 = message2 || least_common
  end

  say message1
  say message2
return
