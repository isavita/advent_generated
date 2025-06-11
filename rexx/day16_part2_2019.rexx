
/* REXX */
call main
exit

main:
  input = linein('input.txt')
  len_input = length(input)
  offset = substr(input, 1, 7)

  total_len = len_input * 10000
  len_needed = total_len - offset

  signal. = 0
  do i = 1 to len_needed
    abs_pos = offset + i
    input_idx = (abs_pos - 1) // len_input + 1
    signal.i = substr(input, input_idx, 1)
  end

  do phase = 1 to 100
    sum = 0
    do i = len_needed to 1 by -1
      sum = sum + signal.i
      signal.i = sum // 10
    end
  end

  result = ''
  do i = 1 to 8
    result = result || signal.i
  end
  say result
  return
