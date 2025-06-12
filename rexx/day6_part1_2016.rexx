
/* Rexx */
main:
  lines.0 = 0
  file = 'input.txt'
  do i = 1 while lines(file) > 0
    lines.i = linein(file)
    lines.0 = i
  end
  call stream file, 'c', 'close'

  message_length = length(lines.1)
  message = ''

  do i = 1 to message_length
    counts. = 0
    max_count = 0
    most_common_char = ''

    do j = 1 to lines.0
      char = substr(lines.j, i, 1)
      counts.char = counts.char + 1

      if counts.char > max_count then do
        max_count = counts.char
        most_common_char = char
      end
    end
    message = message || most_common_char
  end

  say message
exit
