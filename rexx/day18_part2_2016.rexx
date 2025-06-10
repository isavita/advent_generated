
call main
exit

main:
  procedure
  total_rows = 400000
  safe = '.'
  current_row = linein('input.txt')
  safe_count = countstr(safe, current_row)
  row_len = length(current_row)

  do i = 2 to total_rows
    padded_row = safe || current_row || safe
    next_row = ''
    do j = 1 to row_len
      left = substr(padded_row, j, 1)
      right = substr(padded_row, j + 2, 1)
      if left \= right then
        next_row = next_row || '^'
      else do
        next_row = next_row || safe
        safe_count = safe_count + 1
      end
    end
    current_row = next_row
  end

  say safe_count
  return
